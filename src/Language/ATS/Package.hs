{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package
    ( packageCompiler
    , nuke
    , fetchCompiler
    , setupCompiler
    , Version (..)
    ) where

import qualified Codec.Archive.Tar       as Tar
import           Codec.Compression.GZip  (compress, decompress)
import           Control.Monad           (void, when)
import qualified Data.ByteString.Lazy    as BS
import           Data.List               (intercalate)
import           Network.HTTP.Client     hiding (decompress)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Directory
import           System.Environment      (getEnv)
import           System.FilePath.Find    (find)
import           System.Posix.Files
import           System.Process

nuke :: Version -> IO ()
nuke v = do
    putStrLn "Cleaning everything..."
    b <- doesDirectoryExist =<< compilerDir v
    when b
        (removeDirectoryRecursive =<< compilerDir v)

newtype Version = Version [Integer]

instance Show Version where
    show (Version is) = intercalate "." (show <$> is)

-- TODO depend on version
compilerDir :: Version -> IO FilePath
compilerDir v = (++ ("/.atspkg/" ++ show v)) <$> getEnv "HOME"

packageCompiler :: FilePath -> IO ()
packageCompiler directory = do
    files <- find (pure True) (pure True) directory
    bytes <- fmap Tar.write . Tar.pack directory $ fmap (drop $ length (directory :: String) + 1) files
    BS.writeFile (directory ++ ".tar.gz") (compress bytes)

pkgUrl :: Version -> String
pkgUrl v = "https://github.com/vmchale/fastcat/releases/download/0.1.5/ATS2-Postiats-" ++ show v ++ ".tar.gz"

-- "https://downloads.sourceforge.net/project/ats2-lang/ats2-lang/ats2-postiats-" ++ show v ++ "/ATS2-Postiats-" ++ show v ++ ".tgz"

fetchCompiler :: Version -> IO ()
fetchCompiler v = do

    cd <- compilerDir v
    needsSetup <- not <$> doesDirectoryExist cd

    when needsSetup $ do

        putStrLn "Fetching compiler..."
        manager <- newManager tlsManagerSettings
        initialRequest <- parseRequest $ pkgUrl v -- "https://github.com/vmchale/fastcat/releases/download/0.1.5/ATS2-Postiats-0.3.8.tar.gz"
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        putStrLn "Unpacking compiler..."
        Tar.unpack cd . Tar.read . decompress $ response

setupCompiler :: Version -> IO ()
setupCompiler v = do

    putStrLn "configuring compiler..."
    cd <- compilerDir v
    let configurePath = cd ++ "/configure"
    setFileMode configurePath ownerModes
    setFileMode (cd ++ "/autogen.sh") ownerModes
    void $ readCreateProcess ((proc (cd ++ "/autogen.sh") []) { cwd = Just cd }) ""
    void $ readCreateProcess ((proc configurePath ["--prefix", cd]) { cwd = Just cd }) ""

    putStrLn "building compiler..."
    void $ readCreateProcess ((proc "make" []) { cwd = Just cd, std_err = CreatePipe }) ""
    void $ readCreateProcess ((proc "make" ["install"]) { cwd = Just cd, std_err = CreatePipe }) ""
