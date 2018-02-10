{-# OPTIONS_GHC -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wcompat #-}

import           Distribution.CommandLine
import           Distribution.Simple
import           Distribution.Types.HookedBuildInfo

installActions :: IO ()
installActions = foldr (>>) (pure ())
    [ writeManpages "man/atspkg.1" "atspkg.1"
    , writeTheFuck
    , writeBashCompletions "atspkg"
    ]

main :: IO ()
main = defaultMainWithHooks $
    simpleUserHooks { preConf = \_ _ -> installActions >> pure emptyHookedBuildInfo }
