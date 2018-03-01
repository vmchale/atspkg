{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.ATS.Package.Compiler
    ( packageCompiler
    , fetchCompiler
    , setupCompiler
    , cleanAll
    , libInstall
    ) where

import qualified Codec.Archive.Tar       as Tar
import           Codec.Compression.GZip  (compress, decompress)
import           Control.Composition
import           Control.Monad           (when)
import qualified Data.ByteString.Lazy    as BS
import           Data.Dependency
import           Data.FileEmbed
import           Network.HTTP.Client     hiding (decompress)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Directory
import           System.Environment      (getEnv)
import           System.FilePath.Find    (find)
import           System.Posix.Files
import           System.Process
import           System.Process.Ext      (silentCreateProcess)

libatsCfg :: String
libatsCfg = $(embedStringFile "dhall/atslib.dhall")

compilerDir :: Version -> IO FilePath
compilerDir v = makeAbsolute =<< dir
    where dir = (++ ("/.atspkg/" ++ show v)) <$> getEnv "HOME"

packageCompiler :: FilePath -> IO ()
packageCompiler directory = do
    files <- find (pure True) (pure True) directory
    bytes <- fmap Tar.write . Tar.pack directory $ fmap (drop $ length (directory :: String) + 1) files
    BS.writeFile (directory ++ ".tar.gz") (compress bytes)

pkgUrl :: Version -> String
pkgUrl v = "https://github.com/vmchale/atspkg/releases/download/compiler/ATS2-Postiats-" ++ show v ++ ".tar.gz"

withCompiler :: String -> Version -> IO ()
withCompiler s v = putStrLn $ s ++ " compiler v" ++ show v ++ "..."

fetchCompiler :: Version -> IO ()
fetchCompiler v = do

    cd <- compilerDir v
    needsSetup <- not <$> doesDirectoryExist cd

    when needsSetup $ do

        withCompiler "Fetching" v
        manager <- newManager tlsManagerSettings
        initialRequest <- parseRequest $ pkgUrl v
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        withCompiler "Unpacking" v
        Tar.unpack cd . Tar.read . decompress $ response

make :: Version -> FilePath -> IO ()
make v cd =
    withCompiler "Building" v >>
    silentCreateProcess ((proc "make" []) { cwd = Just cd })

libInstall :: FilePath -> String -> IO ()
libInstall cd triple =
    putStrLn "Installing cross libraries..." >>
    writeFile (cd ++ "/atspkg.dhall") libatsCfg >>
    silentCreateProcess ((proc "atspkg" ["install", "--target", triple]) { cwd = Just cd })

install :: Maybe String
        -> Version
        -> FilePath
        -> IO ()
install tgt' v cd =
    withCompiler "Installing" v >>
    silentCreateProcess ((proc "make" ["install"]) { cwd = Just cd }) >>
    maybe mempty (libInstall cd) tgt'

configure :: FilePath -> Version -> FilePath -> IO ()
configure configurePath v cd = do

    withCompiler "Configuring" v

    setFileMode configurePath ownerModes
    setFileMode (cd ++ "/autogen.sh") ownerModes

    silentCreateProcess ((proc (cd ++ "/autogen.sh") []) { cwd = Just cd })

    silentCreateProcess ((proc configurePath ["--prefix", cd]) { cwd = Just cd })

setupCompiler :: Maybe FilePath -> Version -> IO ()
setupCompiler tgt' v = do

    cd <- compilerDir v

    biaxe [configure (cd ++ "/configure"), make, install tgt'] v cd

    writeFile (cd ++ "/done") ""

cleanAll :: IO ()
cleanAll = do
    d <- (++ "/.atspkg") <$> getEnv "HOME"
    b <- doesDirectoryExist d
    when b $ do
        putStrLn "Cleaning everything..."
        removeDirectoryRecursive d
