{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Dependency ( -- * Functions
                                         fetchDeps
                                       -- * Constants
                                       , libcAtomicOps
                                       , libcGC
                                       ) where

import qualified Codec.Archive.Tar                    as Tar
import           Codec.Archive.Zip                    (ZipOption (..), extractFilesFromArchive, toArchive)
import qualified Codec.Compression.GZip               as Gzip
import           Control.Concurrent.ParallelIO.Global
import           Control.Lens
import           Control.Monad
import           Data.ByteString.Lazy                 (ByteString)
import           Data.Maybe                           (fromMaybe)
import           Data.Semigroup                       (Semigroup (..))
import qualified Data.Text.Lazy                       as TL
import           Development.Shake.ATS
import           Dhall
import           Language.ATS.Package.Error
import           Language.ATS.Package.PackageSet
import           Language.ATS.Package.Type
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           System.Directory
import           System.Environment                   (getEnv)
import           System.Posix.Files
import           System.Process

libcAtomicOps :: Version -> ATSDependency
libcAtomicOps v = ATSDependency "atomic-ops" ("atomic-ops-" <> g v) ("https://github.com/ivmai/libatomic_ops/releases/download/v" <> g v <> "/libatomic_ops-" <> g v <> ".tar.gz") v mempty
    where g = TL.pack . show

libcGC :: Version -> ATSDependency
libcGC v = ATSDependency "gc" ("gc-" <> g v) ("https://github.com/ivmai/bdwgc/releases/download/v" <> g v <> "/gc-" <> g v <> ".tar.gz") v ["atomic-ops"]
    where g = TL.pack . show

fetchDeps :: Bool -- ^ Set to 'False' if unsure.
          -> [IO ()] -- ^ Setup steps that can be performed concurrently
          -> [ATSDependency] -- ^ ATS dependencies
          -> [ATSDependency] -- ^ C Dependencies
          -> Bool -- ^ Whether to perform setup anyhow.
          -> IO ()
fetchDeps b setup' deps cdeps b' =
    unless (null deps && null cdeps && b') $ do
        deps' <- join <$> setBuildPlan "ats" deps
        putStrLn "Checking ATS dependencies..."
        d <- (<> "lib/") <$> pkgHome
        let libs' = fmap (buildHelper b) deps'
            unpacked = fmap (over dirLens (TL.pack d <>)) cdeps
            clibs = fmap (buildHelper b) unpacked
        parallel_ (setup' ++ libs' ++ clibs)
        mapM_ (setup (GCC Nothing Nothing)) unpacked

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
clibSetup :: CCompiler -- ^ C compiler
          -> String -- ^ Library name
          -> FilePath -- ^ Filepath to unpack to
          -> IO ()
clibSetup cc' lib' p = do
    subdirs <- allSubdirs p
    configurePath <- fromMaybe (p <> "/configure") <$> findFile subdirs "configure"
    setFileMode configurePath ownerModes
    h <- pkgHome
    let procEnv = Just [("CC", ccToString cc'), ("CFLAGS" :: String, "-I" <> h <> "include"), ("PATH", "/usr/bin:/bin")]
    putStrLn $ "configuring " ++ lib' ++ "..."
    void $ readCreateProcess ((proc configurePath ["--prefix", h]) { cwd = Just p, env = procEnv, std_err = CreatePipe }) ""
    putStrLn $ "building " ++ lib' ++ "..."
    void $ readCreateProcess ((proc "make" []) { cwd = Just p, std_err = CreatePipe }) ""
    putStrLn $ "installing " ++ lib' ++ "..."
    void $ readCreateProcess ((proc "make" ["install"]) { cwd = Just p, std_err = CreatePipe }) ""

setup :: CCompiler -- ^ C compiler to use
      -> ATSDependency -- ^ ATSDependency itself
      -> IO ()
setup cc' (ATSDependency lib' dirName' _ _ _) = do
    lib'' <- (<> TL.unpack lib') <$> pkgHome
    b <- doesFileExist lib''
    unless b $ do
        clibSetup cc' (TL.unpack lib') (TL.unpack dirName')
        writeFile lib'' ""

getCompressor :: Text -> IO (ByteString -> ByteString)
getCompressor s
    | ".tar.gz" `TL.isSuffixOf` s || ".tgz" `TL.isSuffixOf` s = pure Gzip.decompress
    | ".tar" `TL.isSuffixOf` s = pure id
    | otherwise = unrecognized (TL.unpack s)

tarResponse :: Text -> FilePath -> ByteString -> IO ()
tarResponse url' dirName response = do
    compress <- getCompressor url'
    let f = Tar.unpack dirName . Tar.read . compress
    f response

zipResponse :: FilePath -> ByteString -> IO ()
zipResponse dirName response = do
    let options = OptDestination dirName
    extractFilesFromArchive [options] (toArchive response)

buildHelper :: Bool -> ATSDependency -> IO ()
buildHelper b (ATSDependency lib' dirName' url'' _ _) = do

    let (lib, dirName, url') = (lib', dirName', url'') & each %~ TL.unpack

    needsSetup <- not <$> doesDirectoryExist (dirName ++ if b then "/atspkg.dhall" else "")

    when needsSetup $ do

        putStrLn ("Fetching library " ++ lib ++ "...")
        manager <- newManager tlsManagerSettings
        initialRequest <- parseRequest url'
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        putStrLn ("Unpacking library " ++ lib ++ "...")
        if "zip" `TL.isSuffixOf` url'' then
            zipResponse dirName response
                else tarResponse url'' dirName response

        needsMove <- doesDirectoryExist (dirName ++ "/package")
        when needsMove $ do
            renameDirectory (dirName ++ "/package") "tempdir"
            removeDirectoryRecursive dirName
            renameDirectory "tempdir" dirName
