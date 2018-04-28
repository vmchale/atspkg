{
module Language.C.Dependency ( getIncludes
                             , getCDepends
                             ) where

import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class
import System.Directory (doesFileExist)
import Data.Bool (bool)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BSL
import System.FilePath (takeDirectory)
import System.Environment (lookupEnv)
import Control.Monad

}

%wrapper "monad-bytestring"

$special_char = [\\ntrba\"]
$inner_char = [^\\]

@esc_char = \\ $special_char
@string = \" (@esc_char | $inner_char)* \"

@include = "#include" | "#include" $white+ \\\n

tokens :-

    $white+                      ;
    "//".*                       ;

    "/*"                         { \_ _ -> nested_comment }

    @include                     { \_ _ -> alex Include }
    @string                      { tok (\_ s -> alex (StringTok (TL.unpack (decodeUtf8 s)))) }

    $printable                   ;

{

data Token = Include
           | StringTok String
           | End

tok f (p,_,s,_) len = f p (BSL.take len s)

alex :: a -> Alex a
alex = pure

alexEOF :: Alex Token
alexEOF = pure End

-- | Given a 'ByteString' containing C, return a list of filepaths it @#include@s.
getIncludes :: BSL.ByteString -> Either String [FilePath]
getIncludes = fmap extractDeps . lexC

nested_comment :: Alex Token
nested_comment = go 1 =<< alexGetInput

    where go :: Int -> AlexInput -> Alex Token
          go 0 input = alexSetInput input >> alexMonadScan
          go n input =
            case alexGetByte input of
                Nothing -> err input
                Just (c, input') ->
                    case Data.Char.chr (fromIntegral c) of
                        '*' ->
                            case alexGetByte input' of
                                Nothing -> err input'
                                Just (47,input_) -> go (n-1) input_
                                Just (_,input_) -> go n input_
                        '/' ->
                            case alexGetByte input' of
                                Nothing -> err input'
                                Just (c',input_) -> go (addLevel c' $ n) input_
                        _ -> go n input'

          addLevel c' = bool id (+1) (c'==42)

          err (pos,_,_,_) =
            let (AlexPn _ line col) = pos in
                alexError ("Error in nested comment at line " ++ show line ++ ", column " ++ show col)

extractDeps :: [Token] -> [FilePath]
extractDeps [] = mempty
extractDeps (Include:StringTok s:xs) = toInclude s : extractDeps xs
extractDeps (_:xs) = extractDeps xs

toInclude :: String -> FilePath
toInclude = tail . init

lexC :: BSL.ByteString -> Either String [Token]
lexC = flip runAlex loop

loop :: Alex [Token]
loop = do
    tok' <- alexMonadScan
    case tok' of
        End -> pure mempty
        _ -> (tok' :) <$> loop

includes' :: BSL.ByteString -> [FilePath]
includes' = either error id . getIncludes

split :: String -> [String]
split = filter (/= ":") . groupBy g
    where g ':' _ = False
          g _ ':' = False
          g _ _   = True

-- | Get any filepaths that were @#include@-ed in a C source file.
getCDepends :: MonadIO m
            => [FilePath] -- ^ Directories to search in
            -> FilePath -- ^ Path to C source file
            -> m [FilePath]
getCDepends incls src = liftIO $ do
    contents <- BSL.readFile src
    envPath <- fromMaybe mempty <$> lookupEnv "C_INCLUDE_PATH"
    let incl = includes' contents
        dir = takeDirectory src
        allDirs = dir : incls ++ split envPath
    filterM doesFileExist ((++) <$> allDirs <*> incl)

}
