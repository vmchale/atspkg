{
module Language.C.Dependency ( getIncludes
                             ) where

import Data.Bool (bool)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BSL

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

    \.                           ;

{

data Token = Include
           | StringTok String
           | End

tok f (p,_,s,_) len = f p (BSL.take len s)

alex :: a -> Alex a
alex = pure

alexEOF :: Alex Token
alexEOF = pure End

-- | Given a 'ByteString' containing C, return a list of filepaths to depend on.
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

}
