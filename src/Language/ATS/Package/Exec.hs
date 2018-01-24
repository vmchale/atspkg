{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Exec ( exec
                                 , buildAll
                                 ) where

import           Control.Composition
import           Data.Bool                 (bool)
import           Data.Semigroup            (Semigroup (..))
import           Data.Version              hiding (Version (..))
import           Dhall                     hiding (bool)
import           Language.ATS.Package
import           Language.ATS.Package.Type hiding (test, version)
import           Options.Applicative       hiding (auto)
import           Paths_ats_pkg
import           System.Directory          (doesFileExist)
import           System.Environment        (getEnv)

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
    <> command "fetch" (info fetch (progDesc "Fetch and install a binary package"))
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

exec :: IO ()
exec = execParser wrapper >>= run

run :: Command -> IO ()
run c = bool (buildAll >> mkPkg rs) (mkPkg rs) =<< check
    where rs = g c
          g Install    = ["install"]
          g Clean      = ["clean"]
          g (Build ts) = ts
          g Test       = ["test"]
          g _          = []

want :: IO Version
want = Version . compiler <$> input auto "./atspkg.dhall"

buildAll :: IO ()
buildAll = on (>>) (=<< want) fetchCompiler setupCompiler
