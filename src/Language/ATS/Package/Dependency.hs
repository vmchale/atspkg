{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           Data.Semigroup                       (Semigroup (..))
import qualified Data.Text.Lazy                       as TL
import           Dhall
import           Network.HTTP.Client                  hiding (decompress)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           System.Directory
import           System.Environment                   (getEnv)
import           System.Posix.Files
import           System.Process

-- | Type for a dependency
data Dependency = Dependency { libName :: Text -- ^ Library name, e.g.
                             , _dir    :: Text -- ^ Directory we should unpack to
                             , url     :: Text -- ^ Url pointing to tarball
                             }
    deriving (Eq, Show, Generic, Interpret)

makeLenses ''Dependency

fetchDeps :: Bool -- ^ Set to 'False' if unsure.
          -> [Dependency] -- ^ ATS dependencies
          -> [Dependency] -- ^ C Dependencies
          -> IO ()
fetchDeps b deps cdeps =
    unless (null deps) $ do
        putStrLn "Checking ATS dependencies..."
        d <- pkgHome
        let libs = fmap (buildHelper b) deps
            unpacked = fmap (over dir (TL.pack d <>)) cdeps
            clibs = fmap (buildHelper b) unpacked
        parallel_ (libs ++ clibs) >> stopGlobalPool
        mapM_ setup unpacked

pkgHome :: IO FilePath
pkgHome = (++ "/.atspkg/lib/") <$> getEnv "HOME"

clibSetup :: FilePath -> IO ()
clibSetup p = do
    let configurePath = p ++ "/configure"
    setFileMode configurePath ownerModes
    void $ readCreateProcess ((proc p ["--prefix", p]) { cwd = Just p }) ""
    void $ readCreateProcess ((proc "make" []) { cwd = Just p}) ""
    void $ readCreateProcess ((proc "make" ["install"]) { cwd = Just p }) ""

setup :: Dependency -> IO ()
setup (Dependency _ dirName' _) =
    clibSetup (TL.unpack dirName')

buildHelper :: Bool -> Dependency -> IO ()
buildHelper b (Dependency lib' dirName' url'') = do

    let (lib, dirName, url') = (lib', dirName', url'') & each %~ TL.unpack

    needsSetup <- not <$> doesDirectoryExist (dirName ++ if b then "/atspkg.dhall" else "")

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
