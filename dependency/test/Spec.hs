{-# LANGUAGE TupleSections #-}

import           Control.Arrow
import           Data.Dependency
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Test.Hspec

free :: Dependency
free = Dependency "free" mempty defV

comonad :: Dependency
comonad = Dependency "comonad" mempty defV

newLens :: Dependency
newLens = Dependency "lens" ((,mempty) <$> ["free", "comonad"]) (Version [0,2,0])

lens :: Dependency
lens = Dependency "lens" ((,mempty) <$> ["free", "comonad"]) defV

defV :: Version
defV = Version [0,1,0]

ghcMod :: Dependency
ghcMod = Dependency "ghc-mod" [("lens", LessThanEq defV)] defV

deps :: [Dependency]
deps = [free, lens, comonad]

mapSingles :: [(d, b)] -> [(d, S.Set b)]
mapSingles = fmap (second S.singleton)

set :: PackageSet Dependency
set = PackageSet $ M.fromList (("lens", S.fromList [lens, newLens]) : mapSingles [("ghc-mod", ghcMod), ("comonad", comonad), ("free", free)])

main :: IO ()
main = hspec $ parallel $ do
    describe "buildSequence" $
        it "correctly orders dependencies" $
            buildSequence deps `shouldBe` [[free, comonad], [lens]]
    describe "resolveDependencies" $ do
        it "correctly resolves dependencies in a package set" $
            resolveDependencies set [newLens] `shouldBe` Right [[free, comonad], [newLens]]
        it "correctly resolves dependencies in a package set" $
            resolveDependencies set [ghcMod] `shouldBe` Right [[free, comonad], [lens], [ghcMod]]
