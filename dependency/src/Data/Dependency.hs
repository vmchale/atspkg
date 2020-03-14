{-# LANGUAGE TupleSections #-}

module Data.Dependency
    ( -- * Functions
      resolveDependencies
    -- * Types
    , Dependency (..)
    , PackageSet (..)
    , Version (..)
    , DepM
    , ResolveError (..)
    , Constraint (..)
    ) where

import           Control.Arrow
import           Control.Monad
import           Data.Dependency.Error
import           Data.Dependency.Sort
import           Data.Dependency.Type
import           Data.Foldable         (toList)
import           Data.List             (groupBy)
import qualified Data.Map              as M
import qualified Data.Set              as S

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

latest :: PackageSet Dependency -> [Dependency] -> Dependency -> DepM (String, Dependency)
latest (PackageSet ps) ds d@(Dependency ln _ _) =
    (ln,) <$> (lookupSet (Just d) ds =<< lookupMap ln ps)

-- | This splits dependencies into phases
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
    where list = (:) dep <$> (traverse (lookupSet Nothing (crunch ps)) =<< deps)
          deps = sequence [ lookupMap lib ps | lib <- fst <$> _libDependencies dep ]
          crunch = foldMap toList . toList

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
resolveDependencies :: PackageSet Dependency -- ^ Package set
                    -> [Dependency] -- ^ Dependencies requested
                    -> DepM [[Dependency]] -- ^ Phased build
resolveDependencies ps = select . getLatest <=< fmap prepare . saturate
    where select = fmap (fmap (fmap snd))
          saturate = fmap S.unions . traverse (saturateDeps ps)
          prepare = (buildSequence &&& id) . toList
          getLatest (stepped, allDeps) = traverse (traverse (latest ps allDeps)) stepped
