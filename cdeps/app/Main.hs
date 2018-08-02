module Main (main) where

import           Control.Monad
import           Data.Bool             (bool)
import           Data.Foldable
import           Data.Version          (showVersion)
import           Language.C.Dependency
import           Options.Applicative
import           Paths_cdeps           (version)

includes' :: Parser [FilePath]
includes' = many $ strOption
    (long "include"
    <> metavar "DIR"
    <> short 'I'
    <> help "Directories for inclusions."
    <> dirCompletions)

noRecurse :: Parser Bool
noRecurse = switch
    (long "no-recurse"
    <> short 'r'
    <> help "Do not search for dependencies recursively.")

data Command = Dump FilePath [FilePath] Bool

dump :: Parser Command
dump = Dump
    <$> target
    <*> includes'
    <*> noRecurse

dirCompletions :: HasCompleter f => Mod f a
dirCompletions = action "directory"

cCompletions :: HasCompleter f => Mod f a
cCompletions = completer . bashCompleter $ "file -X '!*.c' -o plusdirs"

target :: Parser FilePath
target = argument str
    (metavar "SOURCE"
    <> help "C source file"
    <> cCompletions)

run :: Command -> IO ()
run (Dump cSrc is nr) = (traverse_ putStrLn <=< go is) cSrc
    where go = bool getAll getCDepends nr

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("cdeps version: " ++ showVersion version) (short 'V' <> long "version" <> help "Show version")

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionInfo <*> dump)
    (fullDesc
    <> progDesc "CDeps finds local dependencies for builds"
    <> header "cdeps - a tool for tracking build dependencies")

main :: IO ()
main = execParser wrapper >>= run
