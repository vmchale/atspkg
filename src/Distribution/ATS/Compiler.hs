{-# LANGUAGE OverloadedStrings #-}

-- | This module contains scripts to fetch the compiler.
module Distribution.ATS.Compiler
    ( packageCompiler
    , fetchCompiler
    , setupCompiler
    ) where

import qualified Codec.Archive.Tar       as Tar
import           Codec.Compression.GZip  (compress, decompress)
import           Control.Composition
import           Control.Monad           (when)
import qualified Data.ByteString.Lazy    as BS
import           Data.Dependency
import           Data.Maybe              (fromMaybe)
import           Data.Semigroup
import           Network.HTTP.Client     hiding (decompress)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Directory
import           System.Environment      (getEnv)
import           System.FilePath.Find    (find)
import           System.Posix.Files
import           System.Process
import           System.Process.Ext      (silentCreateProcess)

compilerDir :: Maybe FilePath -> Version -> IO FilePath
compilerDir mp v = makeAbsolute =<< dir
    where def = (++ ("/.atspkg/" ++ show v)) <$> getEnv "HOME"
          dir = fromMaybe <$> def <*> pure ((<> ('/' : show v)) <$> mp)

packageCompiler :: FilePath -> IO ()
packageCompiler directory = do
    files <- find (pure True) (pure True) directory
    bytes <- fmap Tar.write . Tar.pack directory $ fmap (drop $ length (directory :: String) + 1) files
    BS.writeFile (directory ++ ".tar.gz") (compress bytes)

pkgUrl :: Version -> String
pkgUrl v = "https://github.com/vmchale/atspkg/releases/download/compiler/ATS2-Postiats-" ++ show v ++ ".tar.gz"

withCompiler :: String -> Version -> IO ()
withCompiler s v = putStrLn $ s ++ " compiler v" ++ show v ++ "..."

fetchCompiler :: Maybe FilePath -> Version -> IO ()
fetchCompiler mp v = do

    cd <- compilerDir mp v
    needsSetup <- not <$> doesDirectoryExist cd

    when needsSetup $ do

        withCompiler "Fetching" v
        manager <- newManager tlsManagerSettings
        putStrLn $ pkgUrl v
        initialRequest <- parseRequest $ pkgUrl v
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        withCompiler "Unpacking" v
        Tar.unpack cd . Tar.read . decompress $ response

make :: Version -> FilePath -> IO ()
make v cd =
    withCompiler "Building" v >>
    silentCreateProcess ((proc "make" []) { cwd = Just cd })

install :: Version -> FilePath -> IO ()
install v cd =
    withCompiler "Installing" v >>
    silentCreateProcess ((proc "make" ["install"]) { cwd = Just cd })

configure :: FilePath -> Version -> FilePath -> IO ()
configure configurePath v cd = do

    withCompiler "Configuring" v

    setFileMode configurePath ownerModes
    setFileMode (cd ++ "/autogen.sh") ownerModes

    silentCreateProcess ((proc (cd ++ "/autogen.sh") []) { cwd = Just cd })

    silentCreateProcess ((proc configurePath ["--prefix", cd]) { cwd = Just cd })

setupCompiler :: Maybe FilePath -> Version -> IO ()
setupCompiler mp v = do

    cd <- compilerDir mp v

    biaxe [configure (cd ++ "/configure"), make, install] v cd

    writeFile (cd ++ "/done") ""
