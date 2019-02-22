{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module contains functions for installing the @patscc@ compiler. It
-- also includes functions for building @libatslib@.
module Language.ATS.Package.Compiler
    ( fetchCompiler
    , setupCompiler
    , cleanAll
    -- * Types
    , SetupScript
    ) where

import qualified Codec.Archive           as Archive
import           Codec.Compression.GZip  (decompress)
import           Control.Monad
import qualified Data.ByteString.Lazy    as BS
import           Data.Dependency
import           Data.FileEmbed
import           Network.HTTP.Client     hiding (decompress)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Quaalude

libatsCfg :: String
libatsCfg = $(embedStringFile ("dhall" </> "atslib.dhall"))

compilerDir :: Version -> IO FilePath
compilerDir v = makeAbsolute =<< dir
    where dir = getAppUserDataDirectory ("atspkg" </> show v)

pkgUrl :: Version -> String
pkgUrl v = "http://ats-lang.sourceforge.net/IMPLEMENT/Postiats/ATS2-Postiats-" ++ show v ++ ".tgz"

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
        Archive.unpackToDir cd (BS.toStrict $ decompress response)

make :: Verbosity -> Version -> FilePath -> IO ()
make v' v cd =
    withCompiler "Building" v *>
    silentCreateProcess v' ((proc makeExe []) { cwd = Just cd })

type SetupScript = Maybe String -- ^ Optional target triple
                 -> String -- ^ Library name
                 -> FilePath -- ^ File path
                 -> IO ()

libInstall :: SetupScript -> FilePath -> String -> IO ()
libInstall atslibSetup cd triple =
    unless (triple == "musl") $ sequence_
        [ putStrLn "Installing cross libraries..."
        , writeFile (cd </> "atspkg.dhall") libatsCfg
        , atslibSetup (Just triple) "atslib" cd
        ]

install :: Verbosity
        -> Maybe String
        -> SetupScript
        -> Version
        -> FilePath
        -> IO ()
install v' tgt' als v cd =
    withCompiler "Installing" v *>
    silentCreateProcess v' ((proc makeExe ["install"]) { cwd = Just cd }) *>
    maybe mempty (libInstall als cd) tgt'

configure :: Verbosity -> FilePath -> Version -> FilePath -> IO ()
configure v' configurePath v cd = do

    withCompiler "Configuring" v

    makeExecutable configurePath
    makeExecutable (cd </> "autogen.sh")

    silentCreateProcess v' ((proc (cd </> "autogen.sh") []) { cwd = Just cd })

    silentCreateProcess v' ((proc configurePath ["--prefix", cd]) { cwd = Just cd })

setupCompiler :: Verbosity -> SetupScript -> Maybe FilePath -> Version -> IO ()
setupCompiler v' als tgt' v = do

    cd <- compilerDir v

    biaxe [configure v' (cd </> "configure"), make v', install v' tgt' als] v cd

cleanAll :: IO ()
cleanAll = do
    d <- getAppUserDataDirectory "atspkg"
    b <- doesDirectoryExist d
    when b $ do
        putStrLn "Cleaning everything..."
        removeDirectoryRecursive d
