{-# LANGUAGE OverloadedStrings #-}

import           Language.C.Dependency
import           Test.Hspec

main :: IO ()
main = hspec $ parallel $
    describe "byteStringIncludes" $ do
        it "should work (1/2)" $
            getIncludes "#include \"header.h\"" `shouldBe` Right ["header.h"]
        it "should work (2/2)" $
            getIncludes "#include \\\n\"gmp.cats\"" `shouldBe` Right ["gmp.cats"]
