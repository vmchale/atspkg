{-# LANGUAGE OverloadedStrings #-}

module Main ( main
            ) where

import           Control.Concurrent.ParallelIO.Global
import qualified Data.Text.Lazy                       as TL
import           Data.Version                         hiding (Version (..))
import           Development.Shake.ATS
import           Development.Shake.FilePath
import           Dhall.Version                        (dhallVersionString)
import           Distribution.CommandLine
import           GHC.IO.Encoding                      (setLocaleEncoding)
import           Language.ATS                         (languageATSVersion)
import           Language.ATS.Package
import           Language.ATS.Package.Dhall
import           Language.ATS.Package.Upgrade
import           Lens.Micro
import           Options.Applicative
import           Paths_ats_pkg
import           Quaalude                             hiding (command, pack)
import           System.IO                            (utf8)
import           System.IO.Temp                       (withSystemTempDirectory)

-- TODO command to list available packages.
wrapper :: ParserInfo Command
wrapper = info (helper <*> versionInfo <*> command')
    (fullDesc
    <> progDesc "The atspkg build tool for ATS-Postiats."
    <> header "atspkg - a build tool for ATS\nsee 'man atspkg' for more detailed help")

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("atspkg version: " ++ showVersion atspkgVersion ++ "\nlanguage-ats version: " ++ showVersion languageATSVersion ++ "\ndhall version: " ++ dhallVersionString) (short 'V' <> long "version" <> help "Show version")

data Command = Install { _archTarget :: Maybe String
                       , _atspkgArg  :: Maybe String
                       }
             | Build { _targets    :: [String]
                     , _archTarget :: Maybe String
                     , _atspkgArg  :: Maybe String
                     , _rebuildAll :: Bool
                     , _verbosity  :: Int
                     , _lint       :: Bool
                     , _prof       :: Bool
                     }
             | Clean
             | Pack { _target :: String }
             | Test { _targets    :: [String]
                    , _atspkgArg  :: Maybe String
                    , _rebuildAll :: Bool
                    , _verbosity  :: Int
                    , _lint       :: Bool
                    , _prof       :: Bool
                    }
             | Bench { _targets    :: [String]
                     , _atspkgArg  :: Maybe String
                     , _rebuildAll :: Bool
                     , _verbosity  :: Int
                     , _lint       :: Bool
                     , _prof       :: Bool
                     }
             | Fetch { _url       :: String
                     , _atspkgArg :: Maybe String
                     , _verbosity :: Int
                     }
             | Nuke
             | Upgrade
             | Valgrind { _targets   :: [String]
                        , _atspkgArg :: Maybe String
                        }
             | Run { _targets    :: [String]
                   , _atspkgArg  :: Maybe String
                   , _rebuildAll :: Bool
                   , _verbosity  :: Int
                   , _lint       :: Bool
                   , _prof       :: Bool
                   }
             | Check { _filePath :: String, _details :: Bool }
             | CheckSet { _filePath :: String, _details :: Bool }
             | List
             | Setup

userCmd :: Parser Command
userCmd = hsubparser
    (command "install" (info install (progDesc "Install all binaries to $HOME/.local/bin"))
    <> command "clean" (info (pure Clean) (progDesc "Clean current project directory"))
    <> command "remote" (info fetch (progDesc "Fetch and install a binary package"))
    <> command "build" (info build' (progDesc "Build current package targets"))
    <> command "test" (info test' (progDesc "Test current package"))
    <> command "bench" (info bench' (progDesc "Benchmark current package"))
    <> command "nuke" (info (pure Nuke) (progDesc "Uninstall all globally installed libraries"))
    <> command "upgrade" (info (pure Upgrade) (progDesc "Upgrade to the latest version of atspkg"))
    <> command "valgrind" (info valgrind (progDesc "Run generated binaries through valgrind"))
    <> command "run" (info run' (progDesc "Run generated binaries"))
    <> command "check" (info check' (progDesc "Check that a pkg.dhall file is well-typed."))
    <> command "check-set" (info checkSet (progDesc "Audit a package set to ensure it is well-typed."))
    <> command "list" (info (pure List) (progDesc "List available packages"))
    <> command "setup" (info (pure Setup) (progDesc "Install manpages and shell completions."))
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
    <*> pkgArgs

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
    <*> pkgArgs
    <*> rebuild
    <*> verbosity
    <*> noLint
    <*> profile

bench' :: Parser Command
bench' = Bench
    <$> targets "bench"
    <*> pkgArgs
    <*> rebuild
    <*> verbosity
    <*> noLint
    <*> profile

test' :: Parser Command
test' = Test
    <$> targets "test"
    <*> pkgArgs
    <*> rebuild
    <*> verbosity
    <*> noLint
    <*> profile

valgrind :: Parser Command
valgrind = Valgrind
    <$> targets "run with valgrind"
    <*> pkgArgs

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

pkgArgs :: Parser (Maybe String)
pkgArgs = optional
    (strOption
    (long "pkg-args"
    <> help "Arguments to be passed to 'atspkg.dhall'"))

verbosity :: Parser Int
verbosity = length <$>
    many (flag' () (short 'v' <> long "verbose" <> help "Turn up verbosity"))

build' :: Parser Command
build' = Build
    <$> targets "build"
    <*> pkgArgs
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
    <*> pkgArgs
    <*> verbosity

fetchPkg :: Maybe String -> String -> Int -> IO ()
fetchPkg mStr pkg v = withSystemTempDirectory "atspkg" $ \p -> do
    let (dirName, url') = (p, pkg) & each %~ TL.pack
    buildHelper True (ATSDependency mempty dirName url' undefined undefined mempty mempty mempty mempty)
    ps <- getSubdirs p
    pkgDir <- fromMaybe p <$> findFile (p:ps) "atspkg.dhall"
    let setup = [buildAll v mStr Nothing (Just pkgDir)]
    withCurrentDirectory (takeDirectory pkgDir) (mkPkg mStr False False False setup ["install"] Nothing 0)
    stopGlobalPool

main :: IO ()
main = setLocaleEncoding utf8 *>
    execParser wrapper >>= run

runHelper :: Bool -> Bool -> Bool -> [String] -> Maybe String -> Maybe String -> Int -> IO ()
runHelper rba lint tim rs mStr tgt v = g . bool x y . (&& isNothing tgt) =<< check mStr Nothing
    where g xs = mkPkg mStr rba lint tim xs rs tgt v *> stopGlobalPool
          y = mempty
          x = [buildAll v mStr tgt Nothing]

run :: Command -> IO ()
run List                               = displayList "https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/pkgs/pkg-set.dhall"
run (Check p b)                        = void $ ($ Version [0,1,0]) <$> checkPkg p b
run (CheckSet p b)                     = void $ checkPkgSet p b
run Upgrade                            = upgradeBin "vmchale" "atspkg"
run Nuke                               = cleanAll
run (Fetch u mArg v)                   = fetchPkg mArg u v
run Clean                              = mkPkg Nothing False True False mempty ["clean"] Nothing 0
run (Build rs mArg tgt rba v lint tim) = runHelper rba lint tim rs mArg tgt v
run (Test ts mArg rba v lint tim)      = runHelper rba lint tim ("test" : ts) mArg Nothing v
run (Bench ts mArg rba v lint tim)     = runHelper rba lint tim ("bench" : ts) mArg Nothing v
run (Run ts mArg rba v lint tim)       = runHelper rba lint tim ("run" : ts) mArg Nothing v
run (Install tgt mArg)                 = runHelper False True False ["install"] mArg tgt 0
run (Valgrind ts mArg)                 = runHelper False True False ("valgrind" : ts) mArg Nothing 0
run (Pack dir')                        = packageCompiler dir'
run Setup                              = installActions

installActions :: IO ()
installActions = do
    path <- getDataFileName ("man" </> "atspkg.1")
    fold
        [ writeManpages path "atspkg.1"
        , writeTheFuck
        , writeBashCompletions "atspkg"
        ]
