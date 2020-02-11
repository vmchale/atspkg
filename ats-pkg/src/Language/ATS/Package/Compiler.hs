{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module contains functions for installing the @patscc@ compiler. It
-- also includes functions for building @libatslib@.
module Language.ATS.Package.Compiler
    -- ( packageCompiler
    ( fetchCompiler
    , setupCompiler
    , cleanAll
    -- * Types
    , SetupScript
    ) where

import qualified Codec.Archive           as Archive
import           Codec.Compression.GZip  (decompress)
import           Control.Exception       (throw)
import           Control.Monad
import           Data.Dependency
import           Data.FileEmbed
import qualified Development.Shake.Check as Check
import           Network.HTTP.Client     hiding (decompress)
import           Quaalude

libatsCfg :: String
libatsCfg = $(embedStringFile ("dhall" </> "atslib.dhall"))

compilerUnpackDir :: Version -> IO FilePath
compilerUnpackDir v = makeAbsolute =<< dir
    where dir = getAppUserDataDirectory ("atspkg" </> vs)
          vs = show v

compilerDir :: Version -> IO FilePath
compilerDir v = makeAbsolute =<< dir
    where dir = getAppUserDataDirectory ("atspkg" </> vs </> "ATS2-Postiats-gmp-" ++ vs)
          vs = show v

pkgUrl :: Version -> String
pkgUrl v =
    let vs = show v
        in "http://ats-lang.sourceforge.net/IMPLEMENT/Postiats/ATS2-Postiats-gmp-" ++ vs ++ ".tgz"
        -- gmp = if v >= Version [0,3,13] then "gmp-" else ""
        -- in "https://cytranet.dl.sourceforge.net/project/ats2-lang/ats2-lang/ats2-postiats-" ++ vs ++ "/ATS2-Postiats-" ++ gmp ++ vs ++ ".tgz"

-- | Make a tarball from a directory containing the compiler.
-- packageCompiler :: FilePath -> IO ()
-- packageCompiler directory = do
    -- files <- find (pure True) (pure True) directory
    -- bytes <- fmap Tar.write . Tar.pack directory $ fmap (drop $ length (directory :: String) + 1) files
    -- BS.writeFile (directory ++ ".tar.gz") (compress bytes)

withCompiler :: String -> Version -> IO ()
withCompiler s v = putStrLn $ s ++ " compiler v" ++ show v ++ "..."

fetchCompiler :: Version -> IO ()
fetchCompiler v = do

    cd <- compilerUnpackDir v
    needsSetup <- not <$> doesDirectoryExist cd

    when needsSetup $ do

        withCompiler "Fetching" v
        manager <- newManager tlsManagerSettings
        initialRequest <- parseRequest $ pkgUrl v
        response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

        withCompiler "Unpacking" v
        fmap (either throw id) $ Archive.runArchiveM $ Archive.unpackToDirLazy cd (decompress response)

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
    makeExecutable (cd </> "install-sh") *>
    silentCreateProcess v' ((proc makeExe ["install"]) { cwd = Just cd }) *>
    maybe mempty (libInstall als cd) tgt'

configure :: Verbosity -> FilePath -> Version -> FilePath -> IO ()
configure v' configurePath v cd = do

    withCompiler "Configuring" v

    makeExecutable configurePath

    autoconf <- Check.autoconf
    automake <- Check.automake

    when (autoconf && automake) $
        makeExecutable (cd </> "autogen.sh") *>
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
