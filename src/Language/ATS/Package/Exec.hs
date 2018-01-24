module Language.ATS.Package.Exec ( exec
                                 , buildAll
                                 ) where

import           Data.Bool                 (bool)
import           Language.ATS.Package
import           Language.ATS.Package.Type
import           System.Directory          (doesFileExist)
import           System.Environment        (getEnv)

check :: IO Bool
check = do
    home <- getEnv "HOME"
    doesFileExist (home ++ "/.atspkg/bin/patsopt")

exec :: IO ()
exec = bool mkPkg mkPkg =<< check

latest :: Version
latest = Version [0,3,9]

buildAll :: IO ()
buildAll =
    fetchCompiler latest >>
    setupCompiler latest
