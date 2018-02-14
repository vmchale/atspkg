module Main where

import           Control.Arrow
import           Criterion.Main
import           Data.Dependency
import qualified Data.Map        as M
import qualified Data.Set        as S

free :: Dependency
free = Dependency "free" mempty mempty defV

comonad :: Dependency
comonad = Dependency "comonad" mempty mempty defV

lens :: Dependency
lens = Dependency "lens" mempty ["free", "comonad"] defV

defV :: Version
defV = Version [0,1,0]

microlens :: Dependency
microlens = Dependency "microlens" mempty mempty defV

bifunctors :: Dependency
bifunctors = Dependency "bifunctors" mempty ["comonad"] defV

deps :: [Dependency]
deps = [free, lens, comonad]

mapSingles :: [(d, b)] -> [(d, S.Set b)]
mapSingles = fmap (second S.singleton)

set :: PackageSet Dependency
set = PackageSet $ M.fromList (mapSingles [("lens", lens), ("comonad", comonad), ("free", free)])

main :: IO ()
main =
    defaultMain [ bgroup "resolveDependencies"
                      [ bench "3" $ nf (resolveDependencies set) [lens]
                      , bench "3" $ nf (resolveDependencies set) [lens, microlens, bifunctors] ]
                ]
