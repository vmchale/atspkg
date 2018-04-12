-- | Integration with @Cabal@.
module Distribution.ATS.Build ( cabalHooks
                              , atsPolyglotBuild
                              ) where

-- TODO use confHook to set extra-libraries and extra-lib-dirs ourselves?
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Language.ATS.Package.Build
import           Quaalude

-- | Use this in place of 'defaultMain' for a simple build.
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

modifyConf :: FilePath -- ^ New library directory (absolute)
           -> LocalBuildInfo
           -> LocalBuildInfo
modifyConf libDir bi = let old = localPkgDescr bi
    in bi { localPkgDescr = modifyPkgDescr libDir old }

modifyPkgDescr :: String -> PackageDescription -> PackageDescription
modifyPkgDescr libDir pd = let old = library pd
    in pd { library = fmap (modifyLibrary libDir) old }

modifyLibrary :: String -> Library -> Library
modifyLibrary libDir lib = let old = libBuildInfo lib
    in lib { libBuildInfo = modifyBuildInfo libDir old }

-- | Write a dummy file that will allow packaging to work.
writeDummyFile :: IO ()
writeDummyFile =
    createDirectoryIfMissing True "dist-newstyle/lib" >>
    writeFile "dist-newstyle/lib/empty" ""

-- | This uses the users hooks as is @simpleUserHooks@, modified to build the
-- ATS library.
cabalHooks :: UserHooks
cabalHooks = let defConf = confHook simpleUserHooks
    in simpleUserHooks { preConf = (writeDummyFile >>) .* preConf simpleUserHooks
                       , confHook = configureCabal .* defConf }
