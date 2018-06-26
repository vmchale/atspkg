{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable
import qualified Filesystem.Path.CurrentOS as F
import           Language.ATS
import           Test.Hspec
import           Test.Hspec.Dirstream

isATS :: F.FilePath -> Bool
isATS x = (extension x `elem`) (pure <$> ["ats", "dats", "sats", "hats", "cats"])

main :: IO ()
main = hspec $
    describe "pretty-print" $ parallel $
        traverse_ (\x -> testFiles x isATS (fmap printATS . parse))
            ["test/data", "test/data/stdlib", "test/data/stdlib/DATS"]
