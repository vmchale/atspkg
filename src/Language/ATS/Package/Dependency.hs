{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Dependency ( -- * Functions
                                         fetchDeps
                                       -- * Types
                                       , Dependency (..)
                                       ) where

import qualified Codec.Archive.Tar                    as Tar
import           Codec.Compression.GZip               (decompress)
import           Control.Concurrent.ParallelIO.Global
import           Control.Lens
import           Control.Monad
import qualified Data.Text.Lazy                       as TL
import           Dhall
import           Network.HTTP.Client                  hiding (decompress)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           System.Directory

-- | Type for a dependency
data Dependency = Dependency { libName :: Text -- ^ Library name, e.g.
                             , dir     :: Text -- ^ Directory we should unpack to
                             , url     :: Text -- ^ Url pointing to tarball
                             }
    deriving (Eq, Show, Generic, Interpret)

fetchDeps :: [Dependency] -> IO ()
fetchDeps deps =
    unless (null deps) $ do
        putStrLn "Setting up ATS dependencies..."
        let libs = fmap buildHelper deps
        parallel_ libs >> stopGlobalPool

buildHelper :: Dependency -> IO ()
buildHelper (Dependency lib' dirName' url'') = do

    let (lib, dirName, url') = (lib', dirName', url'') & each %~ TL.unpack

    needsSetup <- not <$> doesDirectoryExist (dirName ++ "/atspkg.dhall")

    when needsSetup $ do

        putStrLn ("Fetching library " ++ lib ++ "...")
        manager <- newManager tlsManagerSettings
        initialRequest <- parseRequest url'
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        putStrLn ("Unpacking library " ++ lib ++ "...")
        Tar.unpack dirName . Tar.read . decompress $ response

        putStrLn ("Setting up library " ++ lib ++ "...")
        needsMove <- doesDirectoryExist (dirName ++ "/package")
        when needsMove $ do
            renameDirectory (dirName ++ "/package") "tempdir"
            removeDirectoryRecursive dirName
            renameDirectory "tempdir" dirName
