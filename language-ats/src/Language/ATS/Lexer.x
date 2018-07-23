{

    {-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-incomplete-uni-patterns -fno-warn-unused-imports -fno-warn-orphans -fno-warn-name-shadowing #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE StandaloneDeriving #-}
    {-# LANGUAGE OverloadedStrings #-}

    -- | Module exporting the lexer itself as well as several data types for
    -- working with tokens.
    module Language.ATS.Lexer ( AlexPosn (..)
                              , Token (..)
                              , Keyword (..)
                              , Addendum (..)
                              , lexATS
                              , token_posn
                              , to_string
                              , get_addendum
                              , get_staload
                              ) where

import Data.Char (chr)
import Data.Bool (bool)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Text.PrettyPrint.ANSI.Leijen hiding (line, bool, column, (<$>))

}

%wrapper "monad"

-- Digits
$digit = 0-9
$octal = 0-7
$hex = [0-9 a-f A-F]

-- Characters
$special = [\+\-\*\&\|\[\]\{\}\(\)\_\=\!\%\^\$\@\;\~\,\.\\\#\<\>\:\?]
$alpha = [a-zA-Z]
$terminal = $printable # $white
$esc_char = \27
@escape_ch = \\ ([nrt\'\\] | $octal+)
@escape_str = \\ ([nrt\"\\] | $octal+)
@char = ($terminal # [\\\']) | " " | @escape_ch | $esc_char
@char_lit = \' @char \'

$br = [\<\>]

-- Integer
@integer = $digit+
@unsigned_lit = $digit+ u

-- Floats
@decimals = $digit+
@float = @decimals \. @decimals ("f" | "")

-- Strings
@string = \" ($printable # [\"\\] | @escape_str | $esc_char | \\ \n | \n)* \"

-- Identifiers
@identifier = ($alpha | _) ($alpha | $digit | _ | ! | ')*

-- Multi-line comments
@not_close_paren = (\*+ [^\)] | [^\*] \))
@paren_comment = \(\* ([^\)\*] | @not_close_paren | \n)* \*\)
@not_close_slash = (\*+ [^\/] | [^\*] \/)
@slash_comment = \/\* ([^\/\*] | @not_close_slash | \n)* \*\/
@block_comment = @paren_comment | @slash_comment

@if_block = "#if" (n | "") ("def" | "") ([^\#] | "#then" | "#else" | "#print" | \n)+ "#endif" .*

-- Basically anything that can go inside implicit arguments, i.e. anything
-- necessary to construct a type.
@ref_call = ($alpha | $digit | "(" | ")" | _ | (","))+ ">"

@not_close_c = \% [^\}]
@c_block = \%\{ ("#" | "$" | "^" | "") ([^\%] | @not_close_c | \n)* \%\}

@inner_signature = ("!wrt" | "!exn" | "!refwrt" | "!exnwrt" | "!exnrefwrt" | "!exnref" | "0" | "1" | "!all" | "!laz" | "lin" | "fun" | "clo" | "cloptr" | "cloref" | "!ntm" | "!ref" | "prf" | "fe" | @block_comment)
@inner_signature_mult = (@inner_signature (("," | "") @inner_signature)*) | ""

@lambda = "=>" | "=>>" | "=/=>" | "=<" @inner_signature_mult ">"
@signature = ":<" @inner_signature_mult ">" | ":"
@func_type = "->" | "-<" @inner_signature_mult ">"

@at_brace = \@ ($white | @block_comment)* \{
@at_tuple = \@ ($white | @block_comment)* \(

@box_tuple = \' ($white | @block_comment)* \(
@box_record = \' ($white | @block_comment)* \{

$in_operator = $special # [\)\(\}\{\_\[\]\,]
$in_l = $in_operator # [\<]
$in_r = $in_operator # [\>]
@operator = "+" | "-" | "*" | "/" | "<" | ">" | "=" | "~" | "!" | "?" | "%" | "#[" | $in_r{2,} | $in_l{2,} | $in_r{2,} $in_l+

@double_parens = "(" (@block_comment | $white+ | \n | "//".*)+ ")" | "()"
@double_braces = "{" @block_comment "}" | "{}"
@double_brackets = "<" @block_comment ">" | "<>"

@view = v | view

@fixity_decl = "infixr" | "infixl" | "prefix" | "postfix"

@builtin = \$ (("effmask_" (wrt | all | ref)) | "extfcall" | "extype" (_struct | "") | "arrpsz" | "ldelay" | "delay" | "list" (_vt | "") | "tempenver" | "extval" | "mylocation" | "solver_assert" | "showtype")

@block_comment_start = \(\*
@block_comment_end = \*\)
@c_comment_start = \/\*
@c_comment_end = \*\/

@comment_in = [\*$white]
@comment_general = @comment_in | [\)\(\/]

tokens :-

    <0> $white+                  ;

    -- (sort of) handle nested comments
    <one,two,three,one_c> "(*)"  ;
    <0> @block_comment_start     { tok (\p _ -> alex $ CommentBegin p) `andBegin` one }
    <0> @c_comment_start         { tok (\p _ -> alex $ CommentBegin p) `andBegin` one_c }
    <one> @block_comment_end     { tok (\p _ -> alex $ CommentEnd p) `andBegin` 0 }
    <one> @block_comment_start   { tok (\p s -> alex $ CommentContents p s) `andBegin` two }
    <one> [^\*\(\)]+             { tok (\p s -> alex $ CommentContents p s) }
    <one> @comment_in+ / [^\)]   { tok (\p s -> alex $ CommentContents p s) }
    <one> @comment_general       { tok (\p s -> alex $ CommentContents p s) }
    <two> @block_comment_end     { tok (\p s -> alex $ CommentContents p s) `andBegin` one }
    <two> @block_comment_start   { tok (\p s -> alex $ CommentContents p s) `andBegin` three }
    <two> [^\*\(\)]+             { tok (\p s -> alex $ CommentContents p s) }
    <two> @comment_in+ / [^\)]   { tok (\p s -> alex $ CommentContents p s) }
    <two> @comment_general       { tok (\p s -> alex $ CommentContents p s) }
    <three> @block_comment_end   { tok (\p s -> alex $ CommentContents p s) `andBegin` two }
    <three> @block_comment_start { tok (\p _ -> alexError ("at " <> show (pretty p) <> ": Nested comments of depth > 3 are not supported.")) }
    <three> [^\*\/]+             { tok (\p s -> alex $ CommentContents p s) }
    <three> @comment_in+ / [^\/] { tok (\p s -> alex $ CommentContents p s) }
    <three> @comment_general     { tok (\p s -> alex $ CommentContents p s) }

    -- C-style nested comments
    <one_c> @c_comment_end       { tok (\p _ -> alex $ CommentEnd p) `andBegin` 0 }
    <one_c> @c_comment_start     { tok (\p _ -> alexError ("at " <> show (pretty p) <> ": Nested C comments of depth > 1 are not supported.")) }
    <one_c> [^\*\/]+             { tok (\p s -> alex $ CommentContents p s) }
    <one_c> @comment_in+ / [^\/] { tok (\p s -> alex $ CommentContents p s) }
    <one_c> @comment_general     { tok (\p s -> alex $ CommentContents p s) }

    -- comments and macros
    <0> "//".*                   { tok (\p s -> alex $ CommentLex p s) }
    <0> @c_block                 { tok (\p s -> alex $ CBlockLex p s) }
    <0> "#define".*              { tok (\p s -> alex $ MacroBlock p s) }
    <0> @if_block                { tok (\p s -> alex $ MacroBlock p s) }      

    -- keywords
    <0> fun                      { tok (\p _ -> alex $ Keyword p KwFun) }
    <0> fn                       { tok (\p _ -> alex $ Keyword p KwFn) }
    <0> fnx                      { tok (\p _ -> alex $ Keyword p KwFnx) }
    <0> and                      { tok (\p _ -> alex $ Keyword p KwAnd) }
    <0> prval                    { tok (\p _ -> alex $ Keyword p KwPrval) }
    <0> prvar                    { tok (\p _ -> alex $ Keyword p KwPrvar) }
    <0> prfn                     { tok (\p _ -> alex $ Keyword p KwPrfn) }
    <0> prfun                    { tok (\p _ -> alex $ Keyword p KwPrfun) }
    <0> datatype                 { tok (\p _ -> alex $ Keyword p KwDatatype) }
    <0> data @view type          { tok (\p _ -> alex $ Keyword p KwDatavtype) }
    <0> @view type               { tok (\p _ -> alex $ Keyword p (KwVtype None)) }
    <0> @view type"+"            { tok (\p _ -> alex $ Keyword p (KwVtype Plus)) }
    <0> @view type"-"            { tok (\p _ -> alex $ Keyword p (KwVtype Minus)) }
    <0> dataview                 { tok (\p _ -> alex $ Keyword p KwDataview) }
    <0> dataprop                 { tok (\p _ -> alex $ Keyword p KwDataprop) }
    <0> assume                   { tok (\p _ -> alex $ Keyword p KwAssume) }
    <0> absimpl                  { tok (\p _ -> alex $ Keyword p KwAbsimpl) }
    <0> typedef                  { tok (\p _ -> alex $ Keyword p KwTypedef) }
    <0> @view typedef            { tok (\p _ -> alex $ Keyword p KwVtypedef) }
    <0> absprop                  { tok (\p _ -> alex $ Keyword p KwAbsprop) }
    <0> llam                     { tok (\p _ -> alex $ Keyword p KwLinearLambda) }
    <0> lam                      { tok (\p _ -> alex $ Keyword p KwLambda) }
    <0> staload                  { tok (\p _ -> alex $ Keyword p (KwStaload False)) }
    <0> "#"staload               { tok (\p _ -> alex $ Keyword p (KwStaload True)) }
    <0> dynload                  { tok (\p _ -> alex $ Keyword p (KwDynload False)) }
    <0> "#"dynload               { tok (\p _ -> alex $ Keyword p (KwDynload True)) }
    <0> let                      { tok (\p _ -> alex $ Keyword p KwLet) }
    <0> in                       { tok (\p _ -> alex $ Keyword p KwIn) }
    <0> end                      { tok (\p _ -> alex $ Keyword p KwEnd) }
    <0> case"+"                  { tok (\p _ -> alex $ Keyword p (KwCase Plus)) }
    <0> case"-"                  { tok (\p _ -> alex $ Keyword p (KwCase Minus)) }
    <0> case                     { tok (\p _ -> alex $ Keyword p (KwCase None)) }
    <0> castfn                   { tok (\p _ -> alex $ Keyword p KwCastfn) }
    <0> val"+"                   { tok (\p _ -> alex $ Keyword p (KwVal Plus)) }
    <0> val"-"                   { tok (\p _ -> alex $ Keyword p (KwVal Minus)) }
    <0> val                      { tok (\p _ -> alex $ Keyword p (KwVal None)) }
    <0> var                      { tok (\p _ -> alex $ Keyword p KwVar) }
    <0> if                       { tok (\p _ -> alex $ Keyword p KwIf) }
    <0> sif                      { tok (\p _ -> alex $ Keyword p KwSif) }
    <0> then                     { tok (\p _ -> alex $ Keyword p KwThen) }
    <0> else                     { tok (\p _ -> alex $ Keyword p KwElse) }
    <0> implement                { tok (\p _ -> alex $ Keyword p KwImplement) }
    <0> implmnt                  { tok (\p _ -> alex $ Keyword p KwImplement) }
    <0> primplmnt                { tok (\p _ -> alex $ Keyword p KwProofImplement) }
    <0> primplement              { tok (\p _ -> alex $ Keyword p KwProofImplement) }
    <0> abst"@"ype               { tok (\p _ -> alex $ Keyword p (KwAbst0p None)) }
    <0> abst"@ype+"              { tok (\p _ -> alex $ Keyword p (KwAbst0p Plus)) }
    <0> abst"@type-"             { tok (\p _ -> alex $ Keyword p (KwAbst0p Minus)) }
    <0> abs@view"t@ype"          { tok (\p _ -> alex $ Keyword p (KwAbsvt0p None)) }
    <0> t"@"ype"+"               { tok (\p _ -> alex $ Keyword p (KwT0p Plus)) }
    <0> t"@"ype"-"               { tok (\p _ -> alex $ Keyword p (KwT0p Minus)) }
    <0> t"@"ype                  { tok (\p _ -> alex $ Keyword p (KwT0p None)) }
    <0> @view"t@ype+"            { tok (\p _ -> alex $ Keyword p (KwVt0p Plus)) }
    <0> @view"t@ype-"            { tok (\p _ -> alex $ Keyword p (KwVt0p Minus)) }
    <0> @view"t@ype"             { tok (\p _ -> alex $ Keyword p (KwVt0p None)) }
    <0> abstype                  { tok (\p _ -> alex $ Keyword p KwAbstype) }
    <0> abs @view type           { tok (\p _ -> alex $ Keyword p KwAbsvtype) }
    <0> absview                  { tok (\p _ -> alex $ Keyword p KwAbsview) }
    <0> view                     { tok (\p _ -> alex $ Keyword p (KwView None)) }
    <0> view"+"                  { tok (\p _ -> alex $ Keyword p (KwView Plus)) }
    <0> view"-"                  { tok (\p _ -> alex $ Keyword p (KwView Minus)) }
    <0> viewdef                  { tok (\p _ -> alex $ Keyword p KwViewdef) }
    <0> "#"include               { tok (\p _ -> alex $ Keyword p KwInclude) }
    <0> when                     { tok (\p _ -> alex $ Keyword p KwWhen) }
    <0> of                       { tok (\p _ -> alex $ Keyword p KwOf) }
    <0> ifcase                   { tok (\p _ -> alex $ Keyword p KwIfCase) }
    <0> stadef                   { tok (\p _ -> alex $ Keyword p KwStadef) }
    <0> stacst                   { tok (\p _ -> alex $ Keyword p KwStacst) }
    <0> local                    { tok (\p _ -> alex $ Keyword p KwLocal) }
    <0> praxi                    { tok (\p _ -> alex $ Keyword p KwPraxi) }
    <0> while                    { tok (\p _ -> alex $ Keyword p KwWhile) }
    <0> "while*"                 { tok (\p _ -> alex $ Keyword p KwWhileStar) }
    <0> for                      { tok (\p _ -> alex $ Keyword p KwFor) }
    <0> "for*"                   { tok (\p _ -> alex $ Keyword p KwForStar) }
    <0> where                    { tok (\p _ -> alex $ Keyword p KwWhere) }
    <0> begin                    { tok (\p _ -> alex $ Keyword p KwBegin) }
    <0> overload                 { tok (\p _ -> alex $ Keyword p KwOverload) }
    <0> with                     { tok (\p _ -> alex $ Keyword p KwWith) }
    <0> extern                   { tok (\p _ -> alex $ Keyword p KwExtern) }
    <0> extvar                   { tok (\p _ -> alex $ Keyword p KwExtVar) }
    <0> sortdef                  { tok (\p _ -> alex $ Keyword p KwSortdef) }
    <0> propdef                  { tok (\p _ -> alex $ Keyword p KwPropdef) }
    <0> tkindef                  { tok (\p _ -> alex $ Keyword p KwTKind) }
    <0> typekindef               { tok (\p _ -> alex $ Keyword p KwTKind) }
    <0> "$raise"                 { tok (\p _ -> alex $ Keyword p KwRaise) }
    <0> macdef                   { tok (\p _ -> alex $ Keyword p KwMacdef) }
    <0> mod                      { tok (\p _ -> alex $ Keyword p KwMod) }
    <0> datasort                 { tok (\p _ -> alex $ Keyword p KwDatasort) }
    <0> "println!"               { tok (\p s -> alex $ Identifier p s) }
    <0> "fix@"                   { tok (\p _ -> alex $ Keyword p KwFixAt) }
    <0> "lam@"                   { tok (\p _ -> alex $ Keyword p KwLambdaAt) }
    <0> "addr"                   { tok (\p _ -> alex $ Keyword p KwAddr) }
    <0> "addr@"                  { tok (\p _ -> alex $ Keyword p KwAddrAt) }
    <0> "view@"                  { tok (\p _ -> alex $ Keyword p KwViewAt) }
    <0> sta                      { tok (\p _ -> alex $ Keyword p KwSta) }
    <0> as                       { tok (\p _ -> alex $ Keyword p KwAs) }
    <0> symintr                  { tok (\p _ -> alex $ Keyword p KwSymintr) }
    <0> absview                  { tok (\p _ -> alex $ Keyword p KwAbsview) }
    <0> exception                { tok (\p _ -> alex $ Keyword p KwException) }
    <0> "$list_vt"               { tok (\p _ -> alex $ Keyword p (KwListLit "_vt")) }
    <0> "$list"                  { tok (\p _ -> alex $ Keyword p (KwListLit mempty)) }
    <0> "fold@"                  { tok (\p s -> alex $ Identifier p s) }
    <0> "free@"                  { tok (\p s -> alex $ Identifier p s) }
    <0> @fixity_decl             { tok (\p s -> alex $ FixityTok p s) }

    -- special symbols, literals
    <0> @double_parens           { tok (\p s -> alex $ DoubleParenTok p) }
    <0> @double_braces           { tok (\p s -> alex $ DoubleBracesTok p) }
    <0> @double_brackets         { tok (\p s -> alex $ DoubleBracketTok p) }
    <0> @lambda                  { tok (\p s -> alex $ Arrow p s) }
    <0> @func_type               { tok (\p s -> alex $ FuncType p s) }
    <0> @at_brace                { tok (\p s -> alex $ Special p "@{") }
    <0> @at_tuple                { tok (\p s -> alex $ Special p "@(") }
    <0> @box_tuple               { tok (\p s -> alex $ Special p "'(") }
    <0> @box_record              { tok (\p s -> alex $ Special p "'{") }
    <0> $br / @ref_call          { tok (\p s -> alex $ SpecialBracket p) }
    <0> @signature               { tok (\p s -> alex $ SignatureTok p (tail s)) }
    <0> @operator                { tok (\p s -> alex $ Operator p s) }
    <0> @builtin                 { tok (\p s -> alex $ SpecialIdentifier p (tail s)) }
    <0> $special                 { tok (\p s -> alex $ Special p s) }

    -- literals
    <0> @unsigned_lit            { tok (\p s -> alex $ UintTok p (read $ init s)) }
    <0> @integer                 { tok (\p s -> alex $ IntTok p (read s)) } -- FIXME shouldn't fail silenty on overflow
    <0> "0x" $hex+               { tok (\p s -> alex $ HexIntTok p (drop 2 s)) }
    <0> @float                   { tok (\p s -> alex $ FloatTok p (read s)) }
    <0> @char_lit                { tok (\p s -> alex $ CharTok p (toChar s)) }
    <0> @string                  { tok (\p s -> alex $ StringTok p s) }

    -- identifiers
    <0> @identifier / " "        { tok (\p s -> alex $ IdentifierSpace p s) } -- FIXME get rid of this for performance reasons
    <0> @identifier              { tok (\p s -> alex $ Identifier p s) }

{

nested_comment :: AlexInput -> Int -> Alex Token
nested_comment _ _ = do

    input <- alexGetInput
    go 1 input

    where go :: Int -> AlexInput -> Alex Token
          go 0 input = alexSetInput input >> alexMonadScan
          go n input = do
            case alexGetByte input of
                Nothing -> err input
                Just (c, input) -> do
                    case chr (fromIntegral c) of
                        '*' -> do
                            case alexGetByte input of
                                Nothing -> err input
                                Just (41,input) -> go (n-1) input
                                Just (_,input) -> go n input
                        '(' -> do
                            case alexGetByte input of
                                Nothing -> err input
                                Just (c,input) -> go (bool id (+1) (c==42) $ n) input
                        _ -> go n input

          err (pos,_,_,_) =
            let (AlexPn _ line col) = pos
            in alexError ("Error in nested comment at line " ++ show line ++ ", column " ++ show col)

alex :: a -> Alex a
alex = pure

deriving instance Generic AlexPosn
deriving instance NFData AlexPosn

tok f (p,_,_,s) len = f p (take len s)

-- | Determines the default behavior for incomplete pattern matches
data Addendum = None
              | Plus
              | Minus
              deriving (Eq, Show, Generic, NFData)

-- TODO ideally we'd handle this as an internal error later in the parser.
get_staload (Keyword _ (KwStaload b)) = b
get_staload _ = undefined

get_addendum (Keyword _ (KwVal a)) = a
get_addendum (Keyword _ (KwVtype a)) = a
get_addendum _ = undefined

data Keyword = KwFun
             | KwFnx
             | KwAnd
             | KwDatatype
             | KwDatavtype
             | KwAssume
             | KwAbsimpl
             | KwTypedef
             | KwVtypedef
             | KwVtype Addendum
             | KwStaload Bool
             | KwDynload Bool
             | KwLet
             | KwIn
             | KwLocal
             | KwEnd
             | KwImplement
             | KwCase Addendum
             | KwIf
             | KwSif
             | KwThen
             | KwElse
             | KwVal Addendum
             | KwVar
             | KwLambda
             | KwLinearLambda
             | KwInclude
             | KwWhen
             | KwOf
             | KwAbsprop
             | KwPrval
             | KwPrvar
             | KwStadef
             | KwPraxi
             | KwWhile
             | KwWhileStar
             | KwFor
             | KwForStar
             | KwWhere
             | KwBegin
             | KwOverload
             | KwWith
             | KwIfCase
             | KwDataview
             | KwDataprop
             | KwView Addendum
             | KwAbstype
             | KwType
             | KwAbst0p Addendum
             | KwAbsvt0p Addendum
             | KwT0p Addendum
             | KwVt0p Addendum
             | KwPrfun
             | KwPrfn
             | KwCastfn
             | KwExtern
             | KwAbsvtype
             | KwProofImplement
             | KwSortdef
             | KwExtVar
             | KwPropdef
             | KwRaise
             | KwTKind
             | KwMod
             | KwFixAt
             | KwLambdaAt
             | KwAddrAt
             | KwAddr
             | KwSta
             | KwAs
             | KwViewAt
             | KwViewdef
             | KwSymintr
             | KwAbsview
             | KwFn
             | KwInfix
             | KwInfixr
             | KwInfixl
             | KwStacst
             | KwListLit String
             | KwMacdef
             | KwDatasort
             | KwException
             deriving (Eq, Show, Generic, NFData)

data Token = Identifier AlexPosn String
           | SpecialIdentifier AlexPosn String
           | Keyword AlexPosn Keyword
           | IntTok AlexPosn Int
           | HexIntTok AlexPosn String
           | FloatTok AlexPosn Float
           | CharTok AlexPosn Char
           | StringTok AlexPosn String
           | Special AlexPosn String
           | CBlockLex AlexPosn String
           | IdentifierSpace AlexPosn String
           | Operator AlexPosn String
           | Arrow AlexPosn String
           | FuncType AlexPosn String
           | CommentLex AlexPosn String
           | CommentBegin AlexPosn
           | CommentEnd AlexPosn
           | CommentContents AlexPosn String
           | MacroBlock AlexPosn String
           | UintTok AlexPosn Word
           | SignatureTok AlexPosn String
           | DoubleParenTok AlexPosn
           | DoubleBracesTok AlexPosn
           | DoubleBracketTok AlexPosn
           | SpecialBracket AlexPosn
           | FixityTok AlexPosn String
           | End
           deriving (Eq, Show, Generic, NFData)

instance Pretty Addendum where
    pretty Plus = "+"
    pretty Minus = "-"
    pretty None = ""

maybeSharp b = ((bool "" "#" b) <>)

instance Pretty Keyword where
    pretty KwFun = "fun"
    pretty (KwVtype a) = "vtype" <> pretty a
    pretty KwAnd = "and"
    pretty KwDatatype = "datatype"
    pretty KwDatavtype = "datavtype" -- FIXME this wrongly squashes dataviewtype
    pretty KwFnx = "fnx"
    pretty KwAssume = "assume"
    pretty KwAbsimpl = "absimpl"
    pretty KwTypedef = "typedef"
    pretty KwVtypedef = "vtypedef"
    pretty (KwStaload b) = maybeSharp b "staload"
    pretty (KwDynload b) = maybeSharp b "dynload"
    pretty KwLet = "let"
    pretty KwWhere = "where"
    pretty KwLocal = "local"
    pretty KwEnd = "end"
    pretty KwBegin = "begin"
    pretty KwDatasort = "datasort"
    pretty KwIn = "in"
    pretty KwImplement = "implement"
    pretty (KwCase c) = "case" <> pretty c
    pretty KwIfCase = "ifcase"
    pretty KwIf = "if"
    pretty KwException = "exception"
    pretty KwSif = "sif"
    pretty KwThen = "then"
    pretty KwElse = "else"
    pretty (KwVal c) = "val" <> pretty c
    pretty KwVar = "var"
    pretty KwLambda = "lam"
    pretty KwLinearLambda = "llam"
    pretty KwInclude = "include"
    pretty KwWhen = "when"
    pretty KwOf = "of"
    pretty KwAbsprop = "absprop"
    pretty KwPrval = "prval"
    pretty KwPrvar = "prvar"
    pretty KwStadef = "stadef"
    pretty KwPraxi = "praxi"
    pretty KwWhile = "while"
    pretty KwWhileStar = "while*"
    pretty KwFor = "for"
    pretty KwForStar = "for*"
    pretty KwOverload = "overload"
    pretty KwWith = "with"
    pretty KwDataview = "dataview"
    pretty KwDataprop = "dataprop"
    pretty (KwView c) = "view" <> pretty c
    pretty KwAbstype = "abstype"
    pretty KwAbsvtype = "absvtype"
    pretty KwType = "type"
    pretty (KwAbst0p c) = "abst@ype" <> pretty c
    pretty (KwAbsvt0p c) = "absvt@ype" <> pretty c
    pretty (KwT0p c) = "t@ype" <> pretty c
    pretty (KwVt0p c) = "vt@ype" <> pretty c
    pretty KwPrfun = "prfun"
    pretty KwPrfn = "prfn"
    pretty KwCastfn = "castfn"
    pretty KwExtern = "extern"
    pretty KwRaise = "$raise"
    pretty KwProofImplement = "primplmnt"
    pretty KwSortdef = "sortdef"
    pretty KwPropdef = "propdef"
    pretty KwExtVar = "extvar"
    pretty KwTKind = "tkind"
    pretty KwMod = "mod"
    pretty KwFixAt = "fix@"
    pretty KwLambdaAt = "lam@"
    pretty KwAddrAt = "addr@"
    pretty KwAddr = "addr"
    pretty KwSta = "sta"
    pretty KwAs = "as"
    pretty KwStacst = "stacst"
    pretty KwViewAt = "view@"
    pretty KwViewdef = "viewdef"
    pretty KwSymintr = "symintr"
    pretty KwAbsview = "absview"
    pretty KwFn = "fn"
    pretty KwInfix = "infix"
    pretty KwInfixr = "infixr"
    pretty KwInfixl = "infixl"
    pretty (KwListLit s) = "$list" <> text s
    pretty KwMacdef = "macdef"

instance Pretty Word where
    pretty = text . show

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> ":" <> pretty col

instance Pretty Token where
    pretty (SpecialIdentifier _ s) = text ('$' : s)
    pretty (Identifier _ s) = text s
    pretty (IdentifierSpace _ s) = text s
    pretty (Keyword _ kw) = pretty kw
    pretty (IntTok _ i) = pretty i
    pretty (HexIntTok _ hi) = "0x" <> text hi
    pretty (FloatTok _ x) = pretty x
    pretty (CharTok _ c) = squotes (pretty c)
    pretty (StringTok _ s) = text s
    pretty (Special _ s) = text s
    pretty CBlockLex{} = "%{"
    pretty (Arrow _ s) = text s
    pretty (CommentLex _ s) = text $ take 2 s
    pretty CommentBegin{} = "(*"
    pretty CommentEnd{} = "*)"
    pretty (CommentContents _ s) = text s
    pretty (FuncType _ s) = text s
    pretty (UintTok _ u) = pretty u
    pretty (SignatureTok _ s) = ":" <> text s
    pretty (Operator _ s) = text s
    pretty (MacroBlock _ s) = "#"
    pretty DoubleParenTok{} = "()"
    pretty DoubleBracesTok{} = "{}"
    pretty DoubleBracketTok{} = "<>"
    pretty SpecialBracket{} = "<"
    pretty (FixityTok _ s) = text s
    pretty End = mempty

to_string (CommentLex _ s) = s
to_string (Identifier _ s) = s
to_string (IdentifierSpace _ s) = s
to_string (SpecialIdentifier _ s) = s
to_string (Operator _ s) = s
to_string (CommentContents _ s) = s
to_string _ = undefined

token_posn (SpecialIdentifier p _) = p
token_posn (Identifier p _) = p
token_posn (IdentifierSpace p _) = p
token_posn (Keyword p _) = p
token_posn (IntTok p _) = p
token_posn (FloatTok p _) = p
token_posn (StringTok p _) = p
token_posn (Special p _) = p
token_posn (CBlockLex p _) = p
token_posn (Operator p _) = p
token_posn (Arrow p _) = p
token_posn (FuncType p _) = p
token_posn (CharTok p _) = p
token_posn (CommentLex p _) = p
token_posn (MacroBlock p _) = p
token_posn (UintTok p _) = p
token_posn (SignatureTok p _) = p
token_posn (DoubleParenTok p) = p
token_posn (DoubleBracesTok p) = p
token_posn (DoubleBracketTok p) = p
token_posn (SpecialBracket p) = p
token_posn (FixityTok p _) = p
token_posn (CommentContents p _) = p
token_posn (CommentBegin p) = p
token_posn (CommentEnd p) = p
token_posn (HexIntTok p _) = p
token_posn End = undefined

toChar :: String -> Char
toChar "'\\n'" = '\n'
toChar "'\\t'" = '\t'
toChar "'\\\\'" = '\\'
toChar "'\\0'" = '\0'
toChar x = x !! 1

alexEOF :: Alex Token
alexEOF = pure End

-- | This function turns a string into a stream of tokens for the parser.
lexATS :: String -> Either String [Token]
lexATS str = runAlex str loop

loop :: Alex [Token]
loop = do
    tok' <- alexMonadScan
    if tok' == End then pure []
        else (tok' :) <$> loop

}
