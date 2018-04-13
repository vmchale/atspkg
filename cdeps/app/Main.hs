module Main (main) where

import           Control.Monad
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
    <$> targetP cCompletions id
    <*> includes'

ftypeCompletions :: String -> Mod ArgumentFields a
ftypeCompletions ext = completer . bashCompleter $ "file -X '!*." ++ ext ++ "' -o plusdirs"

cCompletions :: Mod ArgumentFields a
cCompletions = ftypeCompletions "c"

targetP :: Mod ArgumentFields String -> (Parser String -> a) -> a
targetP completions' f = f
    (argument str
    (metavar "SOURCE"
    <> help "C source file"
    <> completions'))

run :: Command -> IO ()
run (Dump cSrc is) = (mapM_ putStrLn <=< getCDepends is) cSrc

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("cdeps version: " ++ showVersion version) (short 'V' <> long "version" <> help "Show version")

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionInfo <*> dump)
    (fullDesc
    <> progDesc "CDeps finds local dependencies for builds"
    <> header "cdeps - a tool for tracking build dependencies")

main :: IO ()
main = execParser wrapper >>= run
