{-# OPTIONS_GHC -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wcompat #-}

import           Data.Bool                       (bool)
import           Data.Foldable                   (fold)
import           Data.Functor                    (($>))
import           Distribution.CommandLine
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.Setup

installActions :: IO ()
installActions = fold
    [ writeManpages "man/atspkg.1" "atspkg.1"
    , writeTheFuck
    , writeBashCompletions "atspkg"
    ]

maybeInstallActions :: ConfigFlags -> IO ()
maybeInstallActions cfs = bool nothing act cond
    where act = installActions
          nothing = mempty
          cond = (mkFlagName "no-executable", True) `notElem` unFlagAssignment (configConfigurationsFlags cfs)

main :: IO ()
main = defaultMainWithHooks $
    simpleUserHooks { preConf = \_ flags -> maybeInstallActions flags $> emptyHookedBuildInfo }
