{-# LANGUAGE OverloadedStrings #-}

import           Language.C.Dependency
import           Test.Hspec

main :: IO ()
main = hspec $
    describe "byteStringIncludes" $
        parallel $ it "should work" $ do
            getIncludes "#include \"header.h\"" `shouldBe` (Right ["header.h"])
            getIncludes "#include \\\n\"gmp.cats\"" `shouldBe` (Right ["gmp.cats"])

