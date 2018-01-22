{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Dependency ( -- * Functions
                                         fetchDeps
                                       -- * Types
                                       , Dependency (..)
                                       -- * Dependency constants
                                       , atsPrelude
                                       , atsIntinf
                                       , atsGMP
                                       ) where

import qualified Codec.Archive.Tar                    as Tar
import           Codec.Compression.GZip               (decompress)
import           Control.Concurrent.ParallelIO.Global
import           Control.Monad
import           Dhall
import           Network.HTTP.Client                  hiding (decompress)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           System.Directory

-- | Type for a dependency
data Dependency = Dependency { _libName :: String -- ^ Library name, e.g.
                             , _dir     :: FilePath -- ^ Directory we should unpack to
                             , _url     :: String -- ^ Url pointing to tarball
                             }
    deriving (Eq, Show, Generic)

atsPrelude :: Dependency
atsPrelude = Dependency "ats2-postiats-0.3.8-prelude" ".atspkg/prelude" "https://downloads.sourceforge.net/project/ats2-lang/ats2-lang/ats2-postiats-0.3.8/ATS2-Postiats-include-0.3.8.tgz"

atsIntinf :: Dependency
atsIntinf = Dependency "atscntrb-hs-intinf-1.0.6" ".atspkg/contrib/atscntrb-hx-intinf" "https://registry.npmjs.org/atscntrb-hx-intinf/-/atscntrb-hx-intinf-1.0.6.tgz"

atsGMP :: Dependency
atsGMP = Dependency "atscntrb-libgmp-1.0.4" ".atspkg/contrib/atscntrb-libgmp" "https://registry.npmjs.org/atscntrb-libgmp/-/atscntrb-libgmp-1.0.4.tgz"

fetchDeps :: [Dependency] -> IO ()
fetchDeps deps = do

    putStrLn "Setting up ATS dependencies..."

    let libs = fmap buildHelper deps

    parallel_ libs >> stopGlobalPool

buildHelper :: Dependency -> IO ()
buildHelper (Dependency libName dirName url) = do

    needsSetup <- not <$> doesDirectoryExist dirName

    when needsSetup $ do

        putStrLn ("Fetching library " ++ libName ++ "...")
        manager <- newManager tlsManagerSettings
        initialRequest <- parseRequest url
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        putStrLn ("Unpacking library " ++ libName ++ "...")
        Tar.unpack dirName . Tar.read . decompress $ response

        putStrLn ("Setting up library " ++ libName ++ "...")
        needsMove <- doesDirectoryExist (dirName ++ "/package")
        when needsMove $ do
            renameDirectory (dirName ++ "/package") "tempdir"
            removeDirectoryRecursive dirName
            renameDirectory "tempdir" dirName
