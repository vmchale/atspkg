{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception            (displayException)
import           Control.Monad                (unless, (<=<))
import           Data.Bifunctor               (first)
import           Data.List                    (lookup)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import qualified Data.Text.IO                 as TIO
import           Data.Version                 (showVersion)
import           Language.ATS
import           Options.Applicative
import           Paths_ats_format
import           System.Directory             (doesFileExist)
import           System.Exit                  (exitFailure)
import           System.IO                    (hPutStr, stderr)
import           System.Process               (readCreateProcess, shell)
import           Text.PrettyPrint.ANSI.Leijen (pretty)
import           TOML

data Program = Program { _path          :: Maybe FilePath
                       , _inplace       :: Bool
                       , _noConfig      :: Bool
                       , _defaultConfig :: Bool
                       }

takeBlock :: String -> (String, String)
takeBlock ('%':'}':ys) = ("", ('%':) . ('}':) $ ys)
takeBlock (y:ys)       = first (y:) $ takeBlock ys
takeBlock []           = ([], [])

rest :: String -> IO String
rest xs = (<> (snd $ takeBlock xs)) <$> printClang (fst $ takeBlock xs)

printClang :: String -> IO String
printClang = readCreateProcess (shell "clang-format")

processClang :: String -> IO String
processClang ('%':'{':'^':xs) = ('%':) . ('{':) . ('^':) <$> rest xs
processClang ('%':'{':'#':xs) = ('%':) . ('{':) . ('#':) <$> rest xs
processClang ('%':'{':'$':xs) = ('%':) . ('{':) . ('$':) <$> rest xs
processClang ('%':'{':xs)     = ('%':) . ('{':) <$> rest xs
processClang (x:xs)           = (x:) <$> processClang xs
processClang []               = pure []

file :: Parser Program
file = Program
    <$> optional (argument str
        (metavar "FILEPATH"
        <> completer (bashCompleter "file -X '!*.*ats' -o plusdirs")
        <> help "File path to ATS source."))
    <*> switch
        (short 'i'
        <> help "Modify file in-place")
    <*> switch
        (long "no-config"
        <> short 'o'
        <> help "Ignore configuration file")
    <*> switch
        (long "default-config"
        <> help "Generate default configuration file in the current directory")

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("atsfmt version: " ++ showVersion version ++ "\nlanguage-ats version: " ++ showVersion languageATSVersion) (short 'V' <> long "version" <> help "Show version")

wrapper :: ParserInfo Program
wrapper = info (helper <*> versionInfo <*> file)
    (fullDesc
    <> progDesc "ATS source code formater. For more detailed help, see 'man atsfmt'"
    <> header "ats-format - a source code formatter written in Haskell")

main :: IO ()
main = execParser wrapper >>= pick

printFail :: String -> IO a
printFail = pure exitFailure <=< hPutStr stderr

defaultConfig :: FilePath -> IO ()
defaultConfig = flip writeFile $
    "ribbon = 0.6 # maximum ribbon fraction\n"
    ++ "width = 120 # maximum width\n"
    ++ "clang-format = false # call clang-format on inline code"

asFloat :: Value -> Maybe Float
asFloat (Double d) = Just (realToFrac d)
asFloat _          = Nothing

asInt :: Value -> Maybe Int
asInt (Integer i) = Just (fromIntegral i)
asInt _           = Nothing

asBool :: Value -> Maybe Bool
asBool (Bool x) = Just x
asBool _        = Nothing

defaults :: (Float, Int, Bool)
defaults = (0.6, 120, False)

parseToml :: FilePath -> IO (Float, Int, Bool)
parseToml fp = do
    f <- TIO.readFile fp
    case parseTOML f of
        Right x -> pure $ fromMaybe defaults $ do
            r <- asFloat =<< lookup "ribbon" x
            w <- asInt =<< lookup "width" x
            cf <- asBool =<< lookup "clang-format" x
            pure (r, w, cf)
        Left err -> printFail $ displayException err

printCustom :: Eq a => ATS a -> IO String
printCustom ats = do
    let p = ".atsfmt.toml"
    config <- doesFileExist p
    if config then do
        (r, w, cf) <- parseToml p
        let t = printATSCustom r w ats
        if cf then
            processClang t
        else
            pure t
    else
        pure $ printATS ats

genErr :: Eq a => Bool -> Either ATSError (ATS a) -> IO ()
genErr b = either (printFail . show . pretty) (putStrLn <=< go)
    where go = if not b then printCustom else pure . printATS

inplace :: FilePath -> (String -> IO String) -> IO ()
inplace p f = do
    contents <- readFile p
    newContents <- f contents
    unless (null newContents) $
        writeFile p newContents

fancyError :: Either ATSError (ATS a) -> IO (ATS a)
fancyError = either (printFail . show . pretty) pure

pick :: Program -> IO ()
pick (Program (Just p) False nc _)  = (genErr nc . parse) =<< readFile p
pick (Program Nothing _ nc False)   = (genErr nc . parse) =<< getContents
pick (Program Nothing _ _ True)     = defaultConfig ".atsfmt.toml"
pick (Program (Just p) True True _) = inplace p (fmap ((<> "\n") . printATS) . fancyError . parse)
pick (Program (Just p) True _ _)    = inplace p (fmap (<> "\n") . printCustom <=< fancyError . parse)
