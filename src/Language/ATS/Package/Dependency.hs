{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Dependency ( -- * Functions
                                         fetchDeps
                                       -- * Types
                                       , Dependency (..)
                                       -- * Constants
                                       , libcAtomicOps
                                       , libcGC
                                       ) where

import qualified Codec.Archive.Tar                    as Tar
import qualified Codec.Compression.GZip               as Gzip
import           Control.Concurrent.ParallelIO.Global
import           Control.Lens
import           Control.Monad
import           Data.ByteString.Lazy                 (ByteString)
import           Data.Maybe                           (fromMaybe)
import           Data.Semigroup                       (Semigroup (..))
import qualified Data.Text.Lazy                       as TL
import           Dhall
import           Language.ATS.Package.Error
import           Language.ATS.Package.Type
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           System.Directory
import           System.Environment                   (getEnv)
import           System.Posix.Files
import           System.Process

libcAtomicOps :: Dependency
libcAtomicOps = Dependency "atomic-ops" "atomic-ops-7.6.2" "https://github.com/ivmai/libatomic_ops/releases/download/v7.6.2/libatomic_ops-7.6.2.tar.gz" (Version [7,6,2])

libcGC :: Version -> Dependency
libcGC v = Dependency "gc" ("gc-" <> g v) ("https://github.com/ivmai/bdwgc/releases/download/v" <> g v <> "/gc-" <> g v <> ".tar.gz") v
    where g = TL.pack . show

fetchDeps :: Bool -- ^ Set to 'False' if unsure.
          -> [Dependency] -- ^ ATS dependencies
          -> [Dependency] -- ^ C Dependencies
          -> IO ()
fetchDeps b deps cdeps =
    unless (null deps && null cdeps) $ do
        putStrLn "Checking ATS dependencies..."
        d <- (<> "lib/") <$> pkgHome
        let libs' = fmap (buildHelper b) deps
            unpacked = fmap (over dirLens (TL.pack d <>)) cdeps
            clibs = fmap (buildHelper b) unpacked
        parallel_ (libs' ++ clibs)
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

-- TODO? autoconf
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
setup (Dependency lib' dirName' _ _) = do
    lib'' <- (<> TL.unpack lib') <$> pkgHome
    b <- doesFileExist lib''
    unless b $ do
        clibSetup (TL.unpack lib') (TL.unpack dirName')
        writeFile lib'' ""

getCompressor :: Text -> IO (ByteString -> ByteString)
getCompressor s
    | ".tar.gz" `TL.isSuffixOf` s || ".tgz" `TL.isSuffixOf` s = pure Gzip.decompress
    | ".tar" `TL.isSuffixOf` s = pure id
    | otherwise = unrecognized (TL.unpack s)

buildHelper :: Bool -> Dependency -> IO ()
buildHelper b (Dependency lib' dirName' url'' _) = do

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
