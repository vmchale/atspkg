module Main (main) where

import           Control.Monad
import           Data.Semigroup hiding (getAll)
import           Data.Version          (showVersion)
import           Language.C.Dependency
import           Options.Applicative
import           Paths_cdeps           (version)

includes' :: Parser [FilePath]
includes' = many $ strOption $ mconcat
    [ long "include"
    , short 'I'
    , help "Directories for inclusions."
    ]

data Command = Dump FilePath [FilePath]

dump :: Parser Command
dump = Dump
    <$> target
    <*> includes'

cCompletions :: Mod ArgumentFields a
cCompletions = completer . bashCompleter $ "file -X '!*.c' -o plusdirs"

target :: Parser FilePath
target = argument str
    (metavar "SOURCE"
    <> help "C source file"
    <> cCompletions)

run :: Command -> IO ()
run (Dump cSrc is) = (mapM_ putStrLn <=< getAll is) cSrc

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("cdeps version: " ++ showVersion version) (short 'V' <> long "version" <> help "Show version")

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionInfo <*> dump)
    (fullDesc
    <> progDesc "CDeps finds local dependencies for builds"
    <> header "cdeps - a tool for tracking build dependencies")

main :: IO ()
main = execParser wrapper >>= run
