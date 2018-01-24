{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Exec ( exec
                                 , buildAll
                                 ) where

import           Control.Composition
import           Control.Lens                    hiding (argument)
import           Data.Bool                       (bool)
import           Data.Maybe                      (fromMaybe)
import           Data.Semigroup                  (Semigroup (..))
import qualified Data.Text.Lazy                  as TL
import           Data.Version                    hiding (Version (..))
import           Development.Shake.FilePath
import           Dhall                           hiding (bool)
import           Language.ATS.Package
import           Language.ATS.Package.Dependency
import           Language.ATS.Package.Type       hiding (test, version)
import           Options.Applicative             hiding (auto)
import           Paths_ats_pkg
import           System.Directory                (doesFileExist, findFile,
                                                  listDirectory,
                                                  withCurrentDirectory)
import           System.Environment              (getEnv)
import           System.IO.Temp                  (withSystemTempDirectory)

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionInfo <*> command')
    (fullDesc
    <> progDesc "The atspkg build tool for ATS-Postiats."
    <> header "atspkg - a build tool for ATS")

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("atspkg version: " ++ showVersion version) (short 'v' <> long "version" <> help "Show version")

data Command = Install
             | Build { _targets :: [String] }
             | Clean
             | Test
             | Fetch { _url :: String }

command' :: Parser Command
command' = hsubparser
    (command "install" (info (pure Install) (progDesc "Install all binaries to $HOME/.local/bin"))
    <> command "clean" (info (pure Clean) (progDesc "Clean current project directory"))
    <> command "remote" (info fetch (progDesc "Fetch and install a binary package"))
    <> command "build" (info build (progDesc "Build current package targets"))
    <> command "test" (info (pure Test) (progDesc "Test current package"))
    )

build :: Parser Command
build = Build <$> many
    (argument str
    (metavar "TARGET"
    <> help "Targets to build"))

fetch :: Parser Command
fetch = Fetch <$>
    argument str
    (metavar "URL"
    <> help "URL pointing to a tarball containing the package to be installed.")

check :: IO Bool
check = do
    home <- getEnv "HOME"
    v <- want
    doesFileExist (home ++ "/.atspkg/" ++ show v ++ "/bin/patscc")

fetchPkg :: String -> IO ()
fetchPkg pkg = withSystemTempDirectory "atspkg" $ \p -> do
    let (lib, dirName, url') = ("atspkg", p, pkg) & each %~ TL.pack
    fetchDeps [Dependency lib dirName url']
    ps <- fmap ((p ++ "/") ++) <$> listDirectory p
    pkgDir <- fromMaybe p <$> findFile (p:ps) "atspkg.dhall"
    withCurrentDirectory (takeDirectory pkgDir) (mkPkg ["install"])

exec :: IO ()
exec = execParser wrapper >>= run

-- https://github.com/vmchale/polyglot/archive/0.3.27.tar.gz
run :: Command -> IO ()
run (Fetch u) = fetchPkg u
run c = bool (buildAll >> mkPkg rs) (mkPkg rs) =<< check
    where rs = g c
          g Install    = ["install"]
          g Clean      = ["clean"]
          g (Build ts) = ts
          g Test       = ["test"]
          g _          = undefined

want :: IO Version
want = Version . compiler <$> input auto "./atspkg.dhall"

buildAll :: IO ()
buildAll = on (>>) (=<< want) fetchCompiler setupCompiler
