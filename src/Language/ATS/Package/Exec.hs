module Language.ATS.Package.Exec ( exec
                                 , buildAll
                                 ) where

import           Data.Bool                 (bool)
import           Language.ATS.Package
import           Language.ATS.Package.Type
import           System.Directory          (doesFileExist)
import           System.Environment        (getEnv)

check :: Version -> IO Bool
check v = do
    home <- getEnv "HOME"
    doesFileExist (home ++ "/.atspkg/" ++ show v ++ "/bin/patscc") -- FIXME version

exec :: IO ()
exec = bool (buildAll >> mkPkg) mkPkg =<< check latest

latest :: Version
latest = Version [0,3,9]

buildAll :: IO ()
buildAll =
    fetchCompiler latest >>
    setupCompiler latest
