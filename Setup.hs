{-# OPTIONS_GHC -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wcompat #-}

import           Distribution.CommandLine
import           Distribution.Simple
import           Distribution.Types.HookedBuildInfo

main :: IO ()
main = defaultMainWithHooks $
    simpleUserHooks { preConf = \_ _ -> writeTheFuck >> writeBashCompletions "atspkg" >> pure emptyHookedBuildInfo }
