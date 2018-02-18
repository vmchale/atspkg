{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Dependency ( -- * Functions
                                         fetchDeps
                                       , buildHelper
                                       ) where

import qualified Codec.Archive.Tar                    as Tar
import           Codec.Archive.Zip                    (ZipOption (..), extractFilesFromArchive, toArchive)
import qualified Codec.Compression.BZip               as Bzip
import qualified Codec.Compression.GZip               as Gzip
import qualified Codec.Compression.Lzma               as Lzma
import           Control.Concurrent.ParallelIO.Global
import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.Text.Lazy                       as TL
import           Development.Shake.ATS
import           Language.ATS.Package.Config
import           Language.ATS.Package.Error
import           Language.ATS.Package.PackageSet
import           Language.ATS.Package.Type
import           Quaalude
import           System.Process

fetchDeps :: CCompiler -- ^ C compiler to use
          -> [IO ()] -- ^ Setup steps that can be performed concurrently
          -> [String] -- ^ ATS dependencies
          -> [String] -- ^ C Dependencies
          -> [String] -- ^ ATS build dependencies
          -> FilePath -- ^ Path to configuration file
          -> Bool -- ^ Whether to perform setup anyhow.
          -> IO ()
fetchDeps cc' setup' deps cdeps atsBld cfgPath b' =
    unless (null deps && null cdeps && b') $ do
        putStrLn "Resolving dependencies..."
        pkgSet <- unpack . defaultPkgs . decode <$> BSL.readFile cfgPath
        deps' <- join <$> setBuildPlan "ats" pkgSet deps
        atsDeps' <- join <$> setBuildPlan "ats-bld" pkgSet atsBld
        putStrLn "Checking ATS dependencies..."
        d <- (<> "lib/") <$> pkgHome cc'
        let libs' = fmap (buildHelper False) deps'
        cdeps' <- join <$> setBuildPlan "c" pkgSet cdeps
        let unpacked = fmap (over dirLens (pack d <>)) cdeps'
            clibs = fmap (buildHelper False) unpacked
            atsLibs = fmap (buildHelper False) atsDeps'
        atsBld' <- join <$> setBuildPlan "atsbld" pkgSet atsBld
        parallel_ (extraWorkerWhileBlocked <$> (setup' ++ libs' ++ clibs ++ atsLibs))
        mapM_ (setup cc') unpacked
        mapM_ atsPkgSetup atsBld'

pkgHome :: CCompiler -> IO FilePath
pkgHome cc' = (++ ("/.atspkg/" ++ ccToDir cc')) <$> getEnv "HOME"

allSubdirs :: FilePath -> IO [FilePath]
allSubdirs [] = pure mempty
allSubdirs d = do
    d' <- listDirectory d
    let d'' = ((d <> "/") <>) <$> d'
    ds <- filterM doesDirectoryExist d''
    ds' <- mapM allSubdirs ds
    pure $ join (ds : ds')

atslibSetup :: String
            -> FilePath
            -> IO ()
atslibSetup lib' p = do
    putStrLn $ "installing " ++ lib' ++ "..."
    subdirs <- allSubdirs p
    pkgPath <- fromMaybe p <$> findFile subdirs "atspkg.dhall"
    readCreateProcess ((proc "atspkg" ["install"]) { cwd = Just (takeDirectory pkgPath), std_err = CreatePipe }) ""

clibSetup :: CCompiler -- ^ C compiler
          -> String -- ^ Library name
          -> FilePath -- ^ Filepath to unpack to
          -> IO ()
clibSetup cc' lib' p = do
    subdirs <- allSubdirs p
    configurePath <- fromMaybe (p <> "/configure") <$> findFile subdirs "configure"
    setFileMode configurePath ownerModes
    h <- pkgHome cc'
    let procEnv = Just [("CC", ccToString cc'), ("CFLAGS" :: String, "-I" <> h <> "include"), ("PATH", "/usr/bin:/bin")]
    putStrLn $ "configuring " ++ lib' ++ "..."
    void $ readCreateProcess ((proc configurePath ["--prefix", h, "--host", host]) { cwd = Just p, env = procEnv, std_err = CreatePipe }) ""
    putStrLn $ "building " ++ lib' ++ "..."
    void $ readCreateProcess ((proc "make" []) { cwd = Just p, std_err = CreatePipe }) ""
    putStrLn $ "installing " ++ lib' ++ "..."
    void $ readCreateProcess ((proc "make" ["install"]) { cwd = Just p, std_err = CreatePipe }) ""

atsPkgSetup :: ATSDependency
            -> IO ()
atsPkgSetup (ATSDependency lib' dirName' _ _ _) = do
    lib'' <- (<> unpack lib') <$> pkgHome GCCStd
    b <- doesFileExist lib''
    unless b $ do
        atslibSetup (unpack lib') (unpack dirName')
        writeFile lib'' ""

setup :: CCompiler -- ^ C compiler to use
      -> ATSDependency -- ^ ATSDependency itself
      -> IO ()
setup cc' (ATSDependency lib' dirName' _ _ _) = do
    lib'' <- (<> unpack lib') <$> pkgHome cc'
    b <- doesFileExist lib''
    unless b $ do
        clibSetup cc' (unpack lib') (unpack dirName')
        writeFile lib'' ""

getCompressor :: Text -> IO (ByteString -> ByteString)
getCompressor s
    | ".tar.gz" `TL.isSuffixOf` s || ".tgz" `TL.isSuffixOf` s = pure Gzip.decompress
    | ".tar" `TL.isSuffixOf` s = pure id
    | ".tar.xz" `TL.isSuffixOf` s = pure Lzma.decompress
    | ".tar.bz2" `TL.isSuffixOf` s = pure Bzip.decompress
    | otherwise = unrecognized (unpack s)

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

    let (lib, dirName, url') = (lib', dirName', url'') & each %~ unpack

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
