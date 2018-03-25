{-# LANGUAGE OverloadedStrings #-}

module Distribution.ATS ( cleanATSCabal
                        , fetchDependencies
                        -- * Types
                        , Version (..)
                        , ATSDependency (..)
                        -- * Libraries
                        , libgmp
                        , intinf
                        , atsPrelude
                        , atsContrib
                        -- * Cabal helper functions
                        , cabalHooks
                        , atsUserHooks
                        , atsPolyglotBuild
                        ) where

import qualified Codec.Archive.Tar                    as Tar
import           Codec.Compression.GZip               (decompress)
import           Control.Concurrent.ParallelIO.Global
import           Control.Monad
import           Data.Bool
import           Data.Dependency
import           Distribution.ATS.Build
import           Distribution.PackageDescription
import           Distribution.Simple                  hiding (Version)
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Network.HTTP.Client                  hiding (decompress)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           System.Directory


data ATSDependency = ATSDependency { _libName  :: String -- ^ Library name
                                   , _filepath :: FilePath -- ^ Directory to unpack library into
                                   , _url      :: String -- ^ URL of tarball containing ATS library.
                                   }

-- | Cleans local build, unless the @development@ flag is set for that
-- particular package.
maybeCleanBuild :: LocalBuildInfo -> IO ()
maybeCleanBuild li =
    let cf = configConfigurationsFlags (configFlags li) in

    unless ((mkFlagName "development", True) `elem` unFlagAssignment cf) $
        putStrLn "Cleaning up ATS dependencies..." >>
        cleanATSCabal

-- | This generates user hooks for a Cabal distribution that has some ATS
-- library dependencies. This will *not* do anything with the ATS source files,
-- but it *will* download any files necessary for the bundled C to compile.
atsUserHooks :: [ATSDependency] -> UserHooks
atsUserHooks deps = simpleUserHooks { preConf = \_ flags -> fetchDependencies flags deps >> pure emptyHookedBuildInfo
                                    , postBuild = \_ _ _ -> maybeCleanBuild
                                    }

-- TODO custom directory?
cleanATSCabal :: IO ()
cleanATSCabal = do
    b <- doesDirectoryExist "ats-deps"
    bool (pure ()) (removeDirectoryRecursive "ats-deps") b

atsContrib :: Version -> ATSDependency
atsContrib v = ATSDependency ("ats2-postiats-" ++ vs ++ "-contrib") "ats-deps/contrib" ("https://downloads.sourceforge.net/project/ats2-lang/ats2-lang/ats2-postiats-" ++ vs ++ "/ATS2-Postiats-contrib-" ++ vs ++ ".tgz")
    where vs = show v

-- | GMP bindings for ATS
libgmp :: Version -> ATSDependency
libgmp v = ATSDependency ("atscntrb-libgmp-" ++ show v) "ats-deps/contrib/atscntrb-libgmp" ("https://registry.npmjs.org/atscntrb-libgmp/-/atscntrb-libgmp-" ++ show v ++ "1.0.4.tgz")

-- | Arbitrary-precision arithmetic library for ATS
intinf :: Version -> ATSDependency
intinf v = ATSDependency ("atscntrb-hs-intinf-" ++ show v) "ats-deps/contrib/atscntrb-hx-intinf" ("https://registry.npmjs.org/atscntrb-hx-intinf/-/atscntrb-hx-intinf-" ++ show v ++ ".tgz")

-- | ATS prelude
atsPrelude :: Version -> ATSDependency
atsPrelude v = ATSDependency ("ats2-postiats-" ++ vs ++ "-prelude") "ats-deps/prelude" ("https://downloads.sourceforge.net/project/ats2-lang/ats2-lang/ats2-postiats-" ++ vs ++ "/ATS2-Postiats-include-" ++ vs ++ ".tgz")
    where vs = show v

fetchDependencies :: ConfigFlags -> [ATSDependency] -> IO ()
fetchDependencies cfs =
    bool act nothing cond
    where act = (>> stopGlobalPool) . parallel_ . fmap fetchDependency
          nothing = pure mempty
          cond = (mkFlagName "with-atsdeps", False) `elem` unFlagAssignment (configConfigurationsFlags cfs)

fetchDependency :: ATSDependency -> IO ()
fetchDependency (ATSDependency libNameATS dirName url) = do

    needsSetup <- not <$> doesFileExist (dirName ++ "/unpacked")

    when needsSetup $ do

        let doing str = putStrLn (str ++ " library " ++ libNameATS ++ "...")
        doing "Fetching"
        manager <- newManager tlsManagerSettings
        initialRequest <- parseRequest url
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        doing "Unpacking"
        Tar.unpack dirName . Tar.read . decompress $ response

        doing "Setting up"
        writeFile (dirName ++ "/unpacked") ""
        needsMove <- doesDirectoryExist (dirName ++ "/package")
        when needsMove $ do
            renameDirectory (dirName ++ "/package") "tempdir"
            removeDirectoryRecursive dirName
            renameDirectory "tempdir" dirName
