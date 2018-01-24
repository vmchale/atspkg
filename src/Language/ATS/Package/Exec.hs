{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Exec ( exec
                                 , buildAll
                                 ) where

import           Data.Bool                 (bool)
import           Dhall                     hiding (bool)
import           Language.ATS.Package
import           Language.ATS.Package.Type
import           System.Directory          (doesFileExist)
import           System.Environment        (getEnv)

check :: IO Bool
check = do
    home <- getEnv "HOME"
    v <- want
    doesFileExist (home ++ "/.atspkg/" ++ show v ++ "/bin/patscc") -- FIXME version

exec :: IO ()
exec = bool (buildAll >> mkPkg) mkPkg =<< check

latest :: Version
latest = Version [0,3,9]

want :: IO Version
want = Version . compiler <$> input auto "./atspkg.dhall"

buildAll :: IO ()
buildAll =
    (fetchCompiler =<< want) >>
    setupCompiler latest
