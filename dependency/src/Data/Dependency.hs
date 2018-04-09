module Data.Dependency
    ( -- * Functions
      resolveDependencies
    , buildSequence
    , check
    -- * Types
    , Dependency (..)
    , PackageSet (..)
    , Version (..)
    , DepM
    , ResolveError (..)
    , Constraint (..)
    , ResolveStateM (..)
    , ResolveState
    , ResMap
    ) where

import           Control.Monad
import           Control.Monad.Tardis
import           Control.Monad.Trans.Class
import           Data.Dependency.Error
import           Data.Dependency.Sort
import           Data.Dependency.Type
import           Data.Foldable             (toList)
import           Data.List                 (groupBy)
import qualified Data.Map                  as M
import qualified Data.Set                  as S

lookupMap :: String -> M.Map String a -> DepM a
lookupMap k ps = case M.lookup k ps of
    Just x  -> Right x
    Nothing -> Left (NotPresent k)

-- | This function checks a package against currently in-scope packages,
-- downgrading versions as necessary until we reach something amenable or run out
-- of options.
checkWith :: Dependency -- ^ Dependency we want
          -> [Dependency] -- ^ Dependencies already in scope.
          -> S.Set Dependency -- ^ Set of available versions
          -> DepM Dependency
checkWith x ds s
    | check x ds = Right x
    | otherwise = lookupSet (Just x) ds (S.deleteMax s)

lookupSet :: Maybe Dependency -- ^ Optional dependency we are looking for.
          -> [Dependency] -- ^ Dependencies already added
          -> S.Set Dependency -- ^ Set of available versions of given dependency
          -> DepM Dependency
lookupSet x ds s = case S.lookupMax s of
    Just x' -> checkWith x' ds s
    Nothing -> g x

    where g Nothing   = Left InternalError
          g (Just x') = Left (Conflicts (_libName <$> ds') (_libName x'))
            where ds' = filter (\d -> not (check x' [d])) ds

-- This does check for compatibility with past packages, but doesn't do the
-- fancy tardis shenanigans it's supposed to when package resolution fails.
latest :: PackageSet Dependency -> Dependency -> ResolveState (String, Dependency)
latest (PackageSet ps) d@(Dependency ln _ _) = do
    st <- getPast
    s <- lift $ lookupMap ln ps
    finish ln (lookupSet (Just d) (toList st) s)

finish :: String -> DepM Dependency -> ResolveState (String, Dependency)
finish ln dep' =
    case dep' of

        Right dep ->
            modifyForwards (M.insert ln dep) >>
            pure (ln, dep)

        Left err -> lift (Left err)

buildSequence :: [Dependency] -> [[Dependency]]
buildSequence = reverse . groupBy independent . sortDeps
    where independent (Dependency ln ls _) (Dependency ln' ls' _) =
            ln' `notElem` (fst <$> ls) && ln `notElem` (fst <$> ls')

iterateM :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
iterateM 0 _ _ = pure []
iterateM n f x = (x:) <$> (iterateM (n-1) f =<< f x)

saturateDeps :: PackageSet Dependency -> Dependency -> DepM (S.Set Dependency)
saturateDeps ps = resolve <=< saturateDeps' ps
    where resolve set = last <$> iterateM n next set
          next depSet = S.unions <$> sequence (saturateDeps' ps <$> toList depSet)
          n = length (toList ps)

saturateDeps' :: PackageSet Dependency -> Dependency -> DepM (S.Set Dependency)
saturateDeps' (PackageSet ps) dep = S.fromList <$> list
    where list = (:) dep <$> (traverse (lookupSet Nothing mempty) =<< deps)
          deps = sequence [ lookupMap lib ps | lib <- fst <$> _libDependencies dep ]

run :: ResolveState a -> DepM a
run = flip evalTardisT (id, mempty) . unResolve

-- | Dependency resolution is guided by the following:
--
-- 1. Always use a newer version when possible
--
-- 2. Obey constraints
--
-- 3. Specify an error for circular dependencies
--
-- 4. Specify an error for overconstrained builds
--
-- 5. Specify an error if a package is not present
--
-- 6. Present a solution whenever one exists.
--
-- This doesn't do any package resolution beyond versioning.
resolveDependencies :: PackageSet Dependency -> [Dependency] -> DepM [[Dependency]]
resolveDependencies ps = select . getLatest <=< fmap (buildSequence . toList) . saturate
    where select = fmap (fmap (fmap snd))
          saturate = fmap S.unions . traverse (saturateDeps ps)
          getLatest = run . traverse (traverse (latest ps))
