module Data.Dependency
    ( -- * Functions
      resolveDependencies
    , buildSequence
    , satisfies
    , compatible
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

checkWith :: Dependency -> [Dependency] -> S.Set Dependency -> DepM Dependency
checkWith x ds s
    | check x ds = Right x
    | otherwise = lookupSet ds (S.deleteMax s)

lookupSet :: [Dependency] -- ^ Dependencies already added
          -> S.Set Dependency -- ^ Set of dependencies
          -> DepM Dependency
lookupSet ds s = case S.lookupMax s of
    Just x  -> checkWith x ds s
    Nothing -> g s

    where g s'
            | S.size s' > 0 = Left (Conflict (_libName <$> ds) (_libName . head . toList $ s'))
            | otherwise = Left InternalError

latest :: PackageSet Dependency -> Dependency -> ResolveState (String, Dependency)
latest (PackageSet ps) (Dependency ln _ _) = do
    st <- getPast
    s <- lift $ lookupMap ln ps
    (,) ln <$> lift (lookupSet (toList st) s)

buildSequence :: [Dependency] -> [[Dependency]]
buildSequence = reverse . groupBy independent . sortDeps
    where independent (Dependency ln ls _) (Dependency ln' ls' _) = ln' `notElem` (fst <$> ls) && ln `notElem` (fst <$> ls')

iterateM :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
iterateM 0 _ _ = pure []
iterateM n f x = (x:) <$> (iterateM (n-1) f =<< f x)

saturateDeps :: PackageSet Dependency -> Dependency -> DepM (S.Set Dependency)
saturateDeps ps = resolve <=< saturateDeps' ps
    where resolve set = last <$> iterateM n next set
          next depSet = S.unions <$> sequence (saturateDeps' ps <$> toList depSet)
          n = length (toList ps)

saturateDeps' :: PackageSet Dependency -> Dependency -> DepM (S.Set Dependency)
saturateDeps' (PackageSet ps) dep = do
    deps <- sequence [ lookupMap lib ps | lib <- fst <$> _libDependencies dep ]
    list <- (:) dep <$> traverse (lookupSet mempty) deps
    pure $ S.fromList list

run :: ResolveState a -> DepM a
run = flip evalTardisT (id, mempty) . unResolve

-- | Heuristics:
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
-- This doesn't do any package resolution beyond versioning.
resolveDependencies :: PackageSet Dependency -> [Dependency] -> DepM [[Dependency]]
resolveDependencies ps = select . getLatest <=< fmap (buildSequence . toList) . saturated
    where select = fmap (fmap (fmap snd))
          saturated dep = S.unions <$> traverse (saturateDeps ps) dep
          getLatest = run . traverse (traverse (latest ps))
