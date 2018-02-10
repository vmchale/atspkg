import           Control.Arrow
import           Data.Dependency
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Test.Hspec

free :: Dependency
free = Dependency "free" mempty mempty defV

comonad :: Dependency
comonad = Dependency "comonad" mempty mempty defV

lens :: Dependency
lens = Dependency "lens" mempty ["free", "comonad"] defV

defV :: Version
defV = Version [0,1,0]

deps :: [Dependency]
deps = [free, lens, comonad]

mapSingles :: [(d, b)] -> [(d, S.Set b)]
mapSingles = fmap (second S.singleton)

set :: PackageSet Dependency
set = PackageSet $ M.fromList (mapSingles [("lens", lens), ("comonad", comonad), ("free", free)])

main :: IO ()
main = hspec $ parallel $ do
    describe "buildSequence" $
        it "correctly orders dependencies" $
            buildSequence deps `shouldBe` [[free, comonad], [lens]]
    describe "resolveDependencies" $
        it "correctly resolves dependencies in a package set" $
            resolveDependencies 10 set [lens] `shouldBe` Right [[free, comonad], [lens]]
