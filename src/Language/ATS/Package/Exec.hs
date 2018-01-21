module Language.ATS.Package.Exec ( exec
                                 , compiler
                                 ) where

import           Data.Bool                 (bool)
import           Language.ATS.Package
import           Language.ATS.Package.Type
import           System.Directory          (doesFileExist)
import           System.Environment        (getEnv)

check :: IO Bool
check = do
    home <- getEnv "HOME"
    doesFileExist (home ++ "/.atspkg/compiler/bin/patsopt")

exec :: IO ()
exec = mkPkg

compiler :: IO ()
compiler = bool buildAll printConfig =<< check

buildAll :: IO ()
buildAll =
    fetchCompiler >>
    setupCompiler
