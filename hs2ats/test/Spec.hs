{-# LANGUAGE OverloadedStrings #-}

import qualified Filesystem.Path.CurrentOS as F
import           Language.ATS.Generate
import           Test.Hspec
import           Test.Hspec.Dirstream

-- process test/data/Pair.out w/ patscc -dd

isATS :: F.FilePath -> Bool
isATS x = (extension x `elem`) (pure <$> ["hs", "hsig", "hs-boot", "x", "y"])

main :: IO ()
main = hspec $
    describe "generateATS" $ parallel $
        testFiles "test/data" isATS (fmap fst . generateATS "<unknown>")
