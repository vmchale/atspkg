{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Exec ( exec
                                 ) where

import           Control.Composition
import           Control.Lens                    hiding (argument)
import           Data.Bool                       (bool)
import           Data.Maybe                      (fromMaybe)
import           Data.Semigroup                  (Semigroup (..))
import qualified Data.Text.Lazy                  as TL
import           Data.Version                    hiding (Version (..))
import           Development.Shake.ATS
import           Development.Shake.FilePath
import           Language.ATS.Package.Build
import           Language.ATS.Package.Compiler
import           Language.ATS.Package.Dependency
import           Options.Applicative             hiding (auto)
import           Paths_ats_pkg
import           System.Directory
import           System.IO.Temp                  (withSystemTempDirectory)

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionInfo <*> command')
    (fullDesc
    <> progDesc "The atspkg build tool for ATS-Postiats."
    <> header "atspkg - a build tool for ATS\nsee 'man atspkg' for more detailed help")

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("atspkg version: " ++ showVersion version) (short 'v' <> long "version" <> help "Show version")

data Command = Install
             | Build { _targets    :: [String]
                     , _recache    :: Bool
                     , _archTarget :: Maybe String
                     , _rebuildAll :: Bool
                     }
             | Clean
             | Test
             | Fetch { _url :: String }
             | Nuke

command' :: Parser Command
command' = hsubparser
    (command "install" (info (pure Install) (progDesc "Install all binaries to $HOME/.local/bin"))
    <> command "clean" (info (pure Clean) (progDesc "Clean current project directory"))
    <> command "remote" (info fetch (progDesc "Fetch and install a binary package"))
    <> command "build" (info build' (progDesc "Build current package targets"))
    <> command "test" (info (pure Test) (progDesc "Test current package"))
    <> command "nuke" (info (pure Nuke) (progDesc "Uninstall all globally installed libraries"))
    )

build' :: Parser Command
build' = Build
    <$> many
        (argument str
        (metavar "TARGET"
        <> help "Targets to build"))
    <*> switch
        (long "no-cache"
        <> short 'c'
        <> help "Turn off configuration caching")
    <*> optional
        (strOption
        (long "target"
        <> short 't'
        <> help "Set target by using its triple"))
    <*> switch
        (long "rebuild"
        <> short 'r'
        <> help "Force rebuild of all targets")

fetch :: Parser Command
fetch = Fetch <$>
    argument str
    (metavar "URL"
    <> help "URL pointing to a tarball containing the package to be installed.")

fetchPkg :: String -> IO ()
fetchPkg pkg = withSystemTempDirectory "atspkg" $ \p -> do
    let (lib, dirName, url') = ("atspkg", p, pkg) & each %~ TL.pack
    fetchDeps True mempty [Dependency lib dirName url' undefined] [] True
    ps <- getSubdirs p
    pkgDir <- fromMaybe p <$> findFile (p:ps) "atspkg.dhall"
    let a = withCurrentDirectory (takeDirectory pkgDir) (mkPkg False False mempty ["install"] Nothing)
    bool (buildAll (Just pkgDir) >> a) a =<< check (Just pkgDir)

exec :: IO ()
exec = execParser wrapper >>= run

runHelper :: Bool -> Bool -> [String] -> Maybe String -> IO ()
runHelper rb rba rs tgt = bool
    (mkPkg rb rba [buildAll Nothing] rs tgt)
    (mkPkg rb rba mempty rs tgt)
    =<< check Nothing

run :: Command -> IO ()
run Nuke = cleanAll
run (Fetch u) = fetchPkg u
run Clean = mkPkg False False mempty ["clean"] Nothing
run (Build rs rb tgt rba) = runHelper rb rba rs tgt
run c = runHelper False False rs Nothing
    where rs = g c
          g Install = ["install"]
          g Test    = ["test"]
          g _       = undefined
