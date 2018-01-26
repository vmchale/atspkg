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
import qualified Codec.Compression.GZip               as Gzip
import qualified Codec.Compression.Lzma               as Lzma
import           Control.Concurrent.ParallelIO.Global
import           Control.Lens
import           Control.Monad
import           Data.ByteString.Lazy                 (ByteString)
import           Data.Maybe                           (fromMaybe)
import           Data.Semigroup                       (Semigroup (..))
import qualified Data.Text.Lazy                       as TL
import           Dhall
import           Language.ATS.Package.Error
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           System.Directory
import           System.Environment                   (getEnv)
import           System.Posix.Files
import           System.Process

-- | Type for a dependency
data Dependency = Dependency { libName :: Text -- ^ Library name, e.g.
                             , dir     :: Text -- ^ Directory we should unpack to
                             , url     :: Text -- ^ Url pointing to tarball
                             }
    deriving (Eq, Show, Generic, Interpret)

makeLensesFor [("dir", "dirLens"), ("libName", "libNameLens")] ''Dependency

fetchDeps :: Bool -- ^ Set to 'False' if unsure.
          -> [Dependency] -- ^ ATS dependencies
          -> [Dependency] -- ^ C Dependencies
          -> IO ()
fetchDeps b deps cdeps =
    unless (null deps) $ do
        putStrLn "Checking ATS dependencies..."
        d <- (<> "lib/") <$> pkgHome
        let libs = fmap (buildHelper b) deps
            unpacked = fmap (over dirLens (TL.pack d <>)) cdeps
            clibs = fmap (buildHelper b . over libNameLens (const "")) unpacked
        parallel_ (libs ++ clibs) >> stopGlobalPool
        mapM_ setup unpacked

pkgHome :: IO FilePath
pkgHome = (++ "/.atspkg/") <$> getEnv "HOME"

allSubdirs :: FilePath -> IO [FilePath]
allSubdirs [] = pure mempty
allSubdirs d = do
    d' <- listDirectory d
    let d'' = ((d <> "/") <>) <$> d'
    ds <- filterM doesDirectoryExist d''
    ds' <- mapM allSubdirs ds
    pure $ join (ds : ds')

clibSetup :: String -> FilePath -> IO ()
clibSetup lib' p = do
    subdirs <- allSubdirs p
    configurePath <- fromMaybe (p <> "/configure") <$> findFile subdirs "configure"
    setFileMode configurePath ownerModes
    h <- pkgHome
    let procEnv = Just [("CFLAGS" :: String, "-I" <> h <> "include"), ("PATH", "/usr/bin:/bin")]
    putStrLn $ "configuring " ++ lib' ++ "..."
    void $ readCreateProcess ((proc configurePath ["--prefix", h]) { cwd = Just p, env = procEnv, std_err = CreatePipe }) ""
    putStrLn $ "building " ++ lib' ++ "..."
    void $ readCreateProcess ((proc "make" []) { cwd = Just p, std_err = CreatePipe }) ""
    putStrLn $ "installing " ++ lib' ++ "..."
    void $ readCreateProcess ((proc "make" ["install"]) { cwd = Just p, std_err = CreatePipe }) ""

setup :: Dependency -> IO ()
setup (Dependency lib' dirName' _) = do
    lib'' <- (<> TL.unpack lib') <$> pkgHome
    b <- doesFileExist lib''
    unless b $ do
        clibSetup (TL.unpack lib') (TL.unpack dirName')
        writeFile lib'' ""

getCompressor :: Text -> IO (ByteString -> ByteString)
getCompressor s
    | ".tar.gz" `TL.isSuffixOf` s = pure Gzip.decompress
    | ".tar.xz" `TL.isSuffixOf` s = pure Lzma.decompress
    | ".tar" `TL.isSuffixOf` s = pure id
    | otherwise = unrecognized (TL.unpack s)

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
        compress <- getCompressor url''
        Tar.unpack dirName . Tar.read . compress $ response

        needsMove <- doesDirectoryExist (dirName ++ "/package")
        when needsMove $ do
            renameDirectory (dirName ++ "/package") "tempdir"
            removeDirectoryRecursive dirName
            renameDirectory "tempdir" dirName
