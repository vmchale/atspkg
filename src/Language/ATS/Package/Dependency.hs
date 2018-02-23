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
import           Language.ATS.Package.Build.IO
import           Language.ATS.Package.Config
import           Language.ATS.Package.Error
import           Language.ATS.Package.PackageSet
import           Language.ATS.Package.Type
import           Quaalude

fetchDeps :: CCompiler -- ^ C compiler to use
          -> [IO ()] -- ^ Setup steps that can be performed concurrently
          -> [String] -- ^ ATS dependencies
          -> [String] -- ^ C Dependencies
          -> [String] -- ^ ATS build dependencies
          -> FilePath -- ^ Path to configuration file
          -> Bool -- ^ Whether to perform setup anyhow.
          -> IO ()
fetchDeps cc' setup' deps cdeps atsBld cfgPath b' =

    unless (null deps && null cdeps && null atsBld && b') $ do

        putStrLn "Resolving dependencies..."

        pkgSet <- unpack . defaultPkgs . decode <$> BSL.readFile cfgPath
        deps' <- setBuildPlan "ats" libDeps pkgSet deps
        atsDeps' <- setBuildPlan "atsbld" libBldDeps pkgSet atsBld
        cdeps' <- setBuildPlan "c" libDeps pkgSet cdeps

        -- Set up actions
        d <- (<> "lib/") <$> pkgHome cc'
        let libs' = fmap (buildHelper False) (join deps')
            unpacked = fmap (over dirLens (pack d <>)) <$> cdeps'
            clibs = fmap (buildHelper False) (join unpacked)
            atsLibs = fmap (buildHelper False) (join atsDeps')
            cBuild = mapM_ (setup cc') <$> transpose unpacked
            atsBuild = mapM_ atsPkgSetup <$> transpose atsDeps'

        -- Fetch all packages & build compiler
        parallel' $ join [ setup', libs', clibs, atsLibs ]

        let tagBuild str bld =
                unless (null bld) $
                    putStrLn (mconcat ["Building ", str, " dependencies..."]) >>
                    parallel' bld

        zipWithM_ tagBuild [ "C", "ATS" ] [ cBuild, atsBuild ]

parallel' :: [IO ()] -> IO ()
parallel' = parallel_ . fmap extraWorkerWhileBlocked

waitCreateProcess :: CreateProcess -> IO ()
waitCreateProcess =
    maybeExit <=< waitForProcess <=< fmap (view _4) . createProcess

atslibSetup :: String
            -> FilePath
            -> IO ()
atslibSetup lib' p = do
    putStrLn $ "installing " ++ lib' ++ "..."
    subdirs <- allSubdirs p
    pkgPath <- fromMaybe p <$> findFile subdirs "atspkg.dhall"
    waitCreateProcess ((proc "atspkg" ["install"]) { cwd = Just (takeDirectory pkgPath), std_out = Inherit })

atsPkgSetup :: ATSDependency
            -> IO ()
atsPkgSetup (ATSDependency lib' dirName' _ _ _ _ _) = do
    lib'' <- (<> unpack lib') <$> pkgHome GCCStd
    b <- doesFileExist lib''
    unless b $ do
        atslibSetup (unpack lib') (unpack dirName')
        writeFile lib'' ""

setup :: CCompiler -- ^ C compiler to use
      -> ATSDependency -- ^ ATSDependency itself
      -> IO ()
setup cc' (ATSDependency lib' dirName' _ _ _ _ _) = do
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
buildHelper b (ATSDependency lib' dirName' url'' _ _ _ _) = do

    let (lib, dirName, url') = (lib', dirName', url'') & each %~ unpack
        isLib = bool "" "library " b

    needsSetup <- not <$> doesDirectoryExist (dirName ++ if b then "/atspkg.dhall" else "")

    when needsSetup $ do

        putStrLn ("Fetching " ++ isLib ++ lib ++ "...")
        manager <- newManager tlsManagerSettings
        initialRequest <- parseRequest url'
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        putStrLn ("Unpacking " ++ isLib ++ lib ++ "...")
        if "zip" `TL.isSuffixOf` url'' then
            zipResponse dirName response
                else tarResponse url'' dirName response

        needsMove <- doesDirectoryExist (dirName ++ "/package")
        when needsMove $ do
            renameDirectory (dirName ++ "/package") "tempdir"
            removeDirectoryRecursive dirName
            renameDirectory "tempdir" dirName
