-- | Integration with @Cabal@.
module Language.ATS.Package.Build.Cabal ( cabalHooks
                                        , atsPolyglotBuild
                                        ) where

import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Language.ATS.Package.Build
import           Quaalude

atsPolyglotBuild :: IO ()
atsPolyglotBuild = defaultMainWithHooks cabalHooks

configureCabal :: IO LocalBuildInfo -> IO LocalBuildInfo
configureCabal = (<*>) $ do
    build mempty
    libDir <- (<> "/") <$> getCurrentDirectory
    pure (modifyConf libDir)

modifyBuildInfo :: String -> BuildInfo -> BuildInfo
modifyBuildInfo libDir bi = let olds = extraLibDirs bi
    in bi { extraLibDirs = (libDir <>) <$> olds }

modifyConf :: String -> LocalBuildInfo -> LocalBuildInfo
modifyConf libDir bi = let old = localPkgDescr bi
    in bi { localPkgDescr = modifyPkgDescr libDir old }

modifyPkgDescr :: String -> PackageDescription -> PackageDescription
modifyPkgDescr libDir pd = let old = library pd
    in pd { library = fmap (modifyLibrary libDir) old }

modifyLibrary :: String -> Library -> Library
modifyLibrary libDir lib = let old = libBuildInfo lib
    in lib { libBuildInfo = modifyBuildInfo libDir old }

writeDummyFile :: IO ()
writeDummyFile =
    writeFile "dist-newstyle/lib" ""

cabalHooks :: UserHooks
cabalHooks = let defConf = confHook simpleUserHooks
    in simpleUserHooks { preConf = \_ _ -> writeDummyFile >> pure emptyHookedBuildInfo
                       , confHook = configureCabal .* defConf }
