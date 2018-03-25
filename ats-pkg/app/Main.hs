{-# LANGUAGE OverloadedStrings #-}

module Main ( main
            ) where

import           Control.Composition
import           Data.Bool                  (bool)
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             (Semigroup (..))
import qualified Data.Text.Lazy             as TL
import           Data.Version               hiding (Version (..))
import           Development.Shake.ATS
import           Development.Shake.FilePath
import           Language.ATS.Package
import           Lens.Micro
import           Options.Applicative
import           System.Directory
import           System.IO.Temp             (withSystemTempDirectory)

-- TODO command to list available packages.
wrapper :: ParserInfo Command
wrapper = info (helper <*> versionInfo <*> command')
    (fullDesc
    <> progDesc "The atspkg build tool for ATS-Postiats."
    <> header "atspkg - a build tool for ATS\nsee 'man atspkg' for more detailed help")

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("atspkg version: " ++ showVersion atspkgVersion) (short 'V' <> long "version" <> help "Show version")

data Command = Install { _archTarget :: Maybe String }
             | Build { _targets    :: [String]
                     , _archTarget :: Maybe String
                     , _rebuildAll :: Bool
                     , _verbosity  :: Int
                     , _lint       :: Bool
                     , _prof       :: Bool
                     }
             | Clean
             | Pack { _target :: String }
             | Test { _targets    :: [String]
                    , _rebuildAll :: Bool
                    , _lint       :: Bool
                    }
             | Fetch { _url :: String }
             | Nuke
             | Upgrade
             | Valgrind { _targets :: [String] }
             | Run { _targets    :: [String]
                   , _rebuildAll :: Bool
                   , _verbosity  :: Int
                   , _lint       :: Bool
                   }
             | Check { _filePath :: String, _details :: Bool }
             | CheckSet { _filePath :: String, _details :: Bool }
             | List

userCmd :: Parser Command
userCmd = hsubparser
    (command "install" (info install (progDesc "Install all binaries to $HOME/.local/bin"))
    <> command "clean" (info (pure Clean) (progDesc "Clean current project directory"))
    <> command "remote" (info fetch (progDesc "Fetch and install a binary package"))
    <> command "build" (info build' (progDesc "Build current package targets"))
    <> command "test" (info test' (progDesc "Test current package"))
    <> command "nuke" (info (pure Nuke) (progDesc "Uninstall all globally installed libraries"))
    <> command "upgrade" (info (pure Upgrade) (progDesc "Upgrade to the latest version of atspkg"))
    <> command "valgrind" (info valgrind (progDesc "Run generated binaries through valgrind"))
    <> command "run" (info run' (progDesc "Run generated binaries"))
    <> command "check" (info check' (progDesc "Check that a pkg.dhall file is well-typed."))
    <> command "check-set" (info checkSet (progDesc "Audit a package set to ensure it is well-typed."))
    <> command "list" (info (pure List) (progDesc "List available packages"))
    )

command' :: Parser Command
command' = userCmd <|> internalCmd

internalCmd :: Parser Command
internalCmd = subparser
    (internal
    <> command "pack" (info pack (progDesc "Make a tarball for distributing the compiler"))
    )

pack :: Parser Command
pack = Pack
    <$> targetP mempty id "package"

install :: Parser Command
install = Install
    <$> triple

checkSet :: Parser Command
checkSet = CheckSet
    <$> targetP dhallCompletions id "check"
    <*> details

check' :: Parser Command
check' = Check
    <$> targetP dhallCompletions id "check-set"
    <*> details

details :: Parser Bool
details = switch
    (long "detailed"
    <> short 'd'
    <> help "Enable detailed error messages")

ftypeCompletions :: String -> Mod ArgumentFields a
ftypeCompletions ext = completer . bashCompleter $ "file -X '!*." ++ ext ++ "' -o plusdirs"

dhallCompletions :: Mod ArgumentFields a
dhallCompletions = ftypeCompletions "dhall"

run' :: Parser Command
run' = Run
    <$> targets "run"
    <*> rebuild
    <*> verbosity
    <*> noLint

test' :: Parser Command
test' = Test
    <$> targets "test"
    <*> rebuild
    <*> noLint

valgrind :: Parser Command
valgrind = Valgrind <$> targets "run with valgrind"

targets :: String -> Parser [String]
targets = targetP mempty many

targetP :: Mod ArgumentFields String -> (Parser String -> a) -> String -> a
targetP completions' f s = f
    (argument str
    (metavar "TARGET"
    <> help ("Targets to " <> s)
    <> completions'))

profile :: Parser Bool
profile = switch
    (long "profile"
    <> short 'p'
    <> help "Print profiling information for the build")

rebuild :: Parser Bool
rebuild = switch
    (long "rebuild"
    <> short 'r'
    <> help "Force rebuild of all targets")

triple :: Parser (Maybe String)
triple = optional
    (strOption
    (long "target"
    <> short 't'
    <> help "Set target by using its triple"))

verbosity :: Parser Int
verbosity = length <$>
    many (flag' () (short 'v' <> long "verbose" <> help "Turn up verbosity"))

build' :: Parser Command
build' = Build
    <$> targets "build"
    <*> triple
    <*> rebuild
    <*> verbosity
    <*> noLint
    <*> profile

noLint :: Parser Bool
noLint = fmap not
    (switch
    (long "no-lint"
    <> short 'l'
    <> help "Disable the shake linter"))

fetch :: Parser Command
fetch = Fetch <$>
    argument str
    (metavar "URL"
    <> help "URL pointing to a tarball containing the package to be installed.")

fetchPkg :: String -> IO ()
fetchPkg pkg = withSystemTempDirectory "atspkg" $ \p -> do
    let (lib, dirName, url') = (mempty, p, pkg) & each %~ TL.pack
    buildHelper True (ATSDependency lib dirName url' undefined undefined mempty mempty mempty)
    ps <- getSubdirs p
    pkgDir <- fromMaybe p <$> findFile (p:ps) "atspkg.dhall"
    let a = withCurrentDirectory (takeDirectory pkgDir) (mkPkg False False False mempty ["install"] Nothing 0)
    bool (buildAll Nothing (Just pkgDir) >> a) a =<< check (Just pkgDir)

main :: IO ()
main = execParser wrapper >>= run

runHelper :: Bool -> Bool -> Bool -> [String] -> Maybe String -> Int -> IO ()
runHelper rba lint tim rs tgt v = g . bool x y =<< check Nothing
    where g xs = mkPkg rba lint tim xs rs tgt v
          y = mempty
          x = [buildAll tgt Nothing]

run :: Command -> IO ()
run List                          = displayList "https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/pkgs/pkg-set.dhall"
run (Check p b)                   = print . ($ Version [0,1,0]) =<< checkPkg p b
run (CheckSet p b)                = print =<< checkPkgSet p b
run Upgrade                       = upgradeBin "vmchale" "atspkg"
run Nuke                          = cleanAll
run (Fetch u)                     = fetchPkg u
run Clean                         = mkPkg False True False mempty ["clean"] Nothing 0
run (Build rs tgt rba v lint tim) = runHelper rba lint tim rs tgt v
run (Test ts rba lint)            = runHelper rba lint False ("test" : ts) Nothing 0
run (Run ts rba v lint)           = runHelper rba lint False ("run" : ts) Nothing v
run (Install tgt)                 = runHelper False True False ["install"] tgt 0
run (Valgrind ts)                 = runHelper False True False ("valgrind" : ts) Nothing 0
run (Pack dir')                   = packageCompiler dir'
