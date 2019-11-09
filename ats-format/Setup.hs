{-# OPTIONS_GHC -Wall #-}

import           Data.Foldable            (sequence_)
import           Distribution.CommandLine
import           Distribution.Simple
import           System.FilePath

main :: IO ()
main = sequence_ [ writeManpages ("man" </> "atsfmt.1") "atsfmt.1"
                 , writeBashCompletions "atsfmt"
                 , setManpathBash
                 , setManpathFish
                 , setManpathZsh
                 , defaultMain
                 ]
