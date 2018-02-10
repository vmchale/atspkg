module Data.Dependency
    ( -- * Functions
      resolveDependencies
    , buildSequence
    -- * Types
    , Dependency (..)
    , PackageSet (..)
    , Version (..)
    , DepM
    , ResolveError (..)
    ) where

import           Control.Composition
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

lookupSet :: S.Set a -> DepM a
lookupSet s = case S.lookupMax s of
    Just x  -> Right x
    Nothing -> Left InternalError

latest :: (Ord a) => PackageSet a -> Dependency -> DepM (String, a)
latest (PackageSet ps) (Dependency ln _ _ _) = do
    s <- lookupMap ln ps
    (,) ln <$> lookupSet s

buildSequence :: [Dependency] -> [[Dependency]]
buildSequence = reverse . groupBy independent . sortDeps
    where independent (Dependency ln _ ls _) (Dependency ln' _ ls' _) = ln' `notElem` ls && ln `notElem` ls'

dock :: (Eq a, Ord a) => [S.Set a] -> S.Set a
dock [x] = x
dock []  = S.fromList []
dock (x:ys@(y:_))
     | x == y = y
     | otherwise = dock ys

iterateM :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
iterateM 0 _ _ = pure []
iterateM n f x = (x:) <$> (iterateM (n-1) f =<< f x)

saturateDeps :: Int -> PackageSet Dependency -> Dependency -> DepM [Dependency]
saturateDeps n ps = fmap toList . resolve <=< saturateDeps' ps
    where resolve :: S.Set Dependency -> DepM (S.Set Dependency)
          resolve set = dock <$> iterateM n next set
          next :: S.Set Dependency -> DepM (S.Set Dependency)
          next depSet = S.unions <$> sequence (saturateDeps' ps <$> toList depSet)

saturateDeps' :: PackageSet Dependency -> Dependency -> DepM (S.Set Dependency)
saturateDeps' (PackageSet ps) dep = do
    let libDeps = _libDependencies dep
    deps <- sequence [ lookupMap lib ps | lib <- libDeps ]
    list <- (:) dep <$> traverse lookupSet deps
    pure $ S.fromList list

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
-- 5. Specify an error if a package is not present.
--
-- This doesn't do any package resolution beyond versioning.
resolveDependencies :: Int -> PackageSet Dependency -> [Dependency] -> DepM [[Dependency]]
resolveDependencies n ps = select . getLatest <=< fmap buildSequence . saturated
    where select = fmap (fmap (fmap snd))
          saturated dep = join <$> traverse (saturateDeps n ps) dep
          getLatest = traverse (traverse (latest ps))
