{
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE DeriveGeneric     #-}
    {-# LANGUAGE DeriveAnyClass    #-}
    {-# LANGUAGE FlexibleContexts  #-}

    -- | This module contains the parser.
    module Language.ATS.Parser ( parseATS
                               , ATSError (..)
                               , preErr
                               ) where

import Control.Composition
import Control.DeepSeq (NFData)
import qualified Data.Map as M
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable (toList)
import GHC.Generics (Generic)
import Language.ATS.Types
import Language.ATS.Types.Lens
import Language.ATS.Lexer ( Token (..)
                          , AlexPosn (..)
                          , Keyword (..)
                          , Addendum (..)
                          , token_posn
                          , to_string
                          , get_addendum
                          , get_staload
                          )
import Lens.Micro (over, _head)
import Prelude
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

}

%name parseATS
%tokentype { Token }
%error { parseError }
%monad { ParseSt } { (>>=) } { pure }

%left plus
%left minus
%left mult
%left div
%left mod
%left percent
%left andOp
%left or
%left identifierSpace
%right at
%right in
%right mutateArrow
%nonassoc lbracket
%nonassoc rbrakcet
%nonassoc neq
%nonassoc doubleEq
%nonassoc doubleBrackets
%nonassoc leq
%nonassoc geq
%nonassoc mutateEq
%nonassoc maybeProof
%nonassoc prfTransform
%nonassoc raise

%token
    fun { Keyword $$ KwFun }
    fn { Keyword $$ KwFn }
    castfn { Keyword $$ KwCastfn }
    prfun { Keyword $$ KwPrfun }
    prfn { Keyword $$ KwPrfn }
    fnx { Keyword $$ KwFnx }
    and { Keyword $$ KwAnd }
    lambda { Keyword $$ KwLambda }
    llambda { Keyword $$ KwLinearLambda }
    if { Keyword $$ KwIf }
    sif { Keyword $$ KwSif }
    stadef { Keyword $$ KwStadef }
    val { $$@(Keyword _ (KwVal _)) }
    prval { Keyword $$ KwPrval }
    prvar { Keyword $$ KwPrvar }
    var { Keyword $$ KwVar }
    then { Keyword $$ KwThen }
    let { Keyword $$ KwLet }
    typedef { Keyword $$ KwTypedef }
    vtypedef { Keyword $$ KwVtypedef }
    absview { Keyword $$ KwAbsview }
    absvtype { Keyword $$ KwAbsvtype }
    abstype { Keyword $$ KwAbstype }
    abst0p { Keyword $$ (KwAbst0p None) }
    absvt0p { Keyword $$ (KwAbsvt0p None) }
    viewdef { Keyword $$ KwViewdef }
    in { Keyword $$ KwIn }
    end { Keyword $$ KwEnd }
    implement { Keyword $$ KwImplement }
    addr { Keyword $$ KwAddr }
    primplmnt { Keyword $$ KwProofImplement }
    else { Keyword $$ KwElse }
    when { Keyword $$ KwWhen }
    begin { Keyword $$ KwBegin }
    case { Keyword _ (KwCase $$) }
    ifcase { Keyword $$ KwIfCase }
    datatype { Keyword $$ KwDatatype }
    dataview { Keyword $$ KwDataview }
    datavtype { Keyword $$ KwDatavtype }
    while { Keyword $$ KwWhile }
    whileStar { Keyword $$ KwWhileStar }
    for { Keyword $$ KwFor }
    forStar { Keyword $$ KwForStar }
    of { Keyword $$ KwOf }
    exception { Keyword $$ KwException }
    include { Keyword $$ KwInclude }
    staload { $$@(Keyword _ (KwStaload _)) }
    dynload { $$@(Keyword _ (KwDynload _)) }
    overload { Keyword $$ KwOverload }
    with { Keyword $$ KwWith }
    dataprop { Keyword $$ KwDataprop }
    praxi { Keyword $$ KwPraxi }
    extvar { Keyword $$ KwExtVar }
    extern { Keyword $$ KwExtern }
    t0pPlain { Keyword $$ (KwT0p None) }
    t0pCo { Keyword $$ (KwT0p Plus) }
    vt0pCo { Keyword $$ (KwVt0p Plus) }
    vt0pPlain { Keyword $$ (KwVt0p None) }
    where { Keyword $$ KwWhere }
    absprop { Keyword $$ KwAbsprop }
    sortdef { Keyword $$ KwSortdef }
    local { Keyword $$ KwLocal }
    view { Keyword $$ (KwView None) }
    viewContra { Keyword $$ (KwView Minus) }
    viewCo { Keyword $$ (KwView Plus) }
    vtype { $$@(Keyword _ KwVtype{}) }
    viewPlusMinus { Keyword _ (KwView $$) }
    raise { Keyword $$ KwRaise }
    tkindef { Keyword $$ KwTKind }
    as { Keyword $$ KwAs }
    assume { Keyword $$ KwAssume }
    absimpl { Keyword $$ KwAbsimpl }
    addrAt { Keyword $$ KwAddrAt }
    viewAt { Keyword $$ KwViewAt }
    symintr { Keyword $$ KwSymintr }
    stacst { Keyword $$ KwStacst }
    propdef { Keyword $$ KwPropdef }
    list_vt { Keyword $$ (KwListLit "_vt") }
    list { Keyword $$ (KwListLit "") }
    datasort { Keyword $$ KwDatasort }
    uintLit { UintTok _ $$ }
    intLit { IntTok _ $$ }
    hexLit { HexIntTok _ $$ }
    floatLit { FloatTok _ $$ }
    specialIdentifier { $$@SpecialIdentifier{} }
    foldAt { Identifier $$ "fold@" }
    identifier { $$@Identifier{} }
    identifierSpace { $$@IdentifierSpace{} }
    closeParen { Special $$ ")" }
    openParen { Special $$ "(" }
    boxTuple { Special $$ "'(" }
    boxRecord { Special $$ "'{" }
    colon { SignatureTok $$ "" }
    signature { SignatureTok _ $$ }
    comma { Special $$ "," }
    percent { Operator $$ "%" }
    geq { Operator $$ ">=" }
    leq { Operator $$ "<=" }
    neq { Operator $$ "!=" }
    openTermetric { Operator $$ ".<" }
    closeTermetric { Operator $$ ">." }
    mutateArrow { FuncType $$ "->" }
    mutateEq { Operator $$ ":=" }
    lbracket { Special $$ "<" }
    rbracket { Special $$ ">" }
    eq { Operator $$ "=" }
    or { Operator $$ "||" }
    vbar { Special $$ "|" }
    lbrace { Special $$ "{" }
    rbrace { Special $$ "}" }
    funcArrow { FuncType _ $$ }
    plainArrow { Arrow $$ "=>" }
    cloref1Arrow { Arrow $$ "=<cloref1>" }
    cloptr1Arrow { Arrow $$ "=<cloptr1>" }
    lincloptr1Arrow { Arrow $$ "=<lincloptr1>" }
    lincloref1Arrow { Arrow $$ "=<lin,cloref1>" }
    proofLambda { Arrow $$ "=<prf>" }
    proofLinearLambda { Arrow $$ "=<lin,prf>" }
    spear { Arrow $$ "=>>" }
    proofArrow { Arrow $$ "=/=>" }
    proofSpear { Arrow $$ "=/=>>" }
    lsqbracket { Special $$ "[" }
    rsqbracket { Special $$ "]" }
    doubleSqBrackets { Special $$ "[]" }
    string { StringTok _ $$ }
    charLit { CharTok _ $$ }
    underscore { Special $$ "_" }
    minus { Operator $$ "-" }
    plus { Operator $$ "+" }
    div { Operator $$ "/" }
    mult { Operator $$ "*" }
    exclamation { Operator $$ "!" }
    dot { Special $$ "." }
    at { Special $$ "@" }
    tilde { Operator $$ "~" }
    backslash { Special $$ "\\" }
    dollar { Special $$ "$" }
    semicolon { Special $$ ";" }
    andOp { Operator $$ "&&" }
    doubleEq { Operator $$ "==" }
    doubleDot { Operator $$ ".." }
    doubleParens { DoubleParenTok $$ }
    doubleBraces { DoubleBracesTok $$ }
    doubleBrackets { DoubleBracketTok $$ }
    prfTransform { Operator $$ ">>" } -- For types like &a >> a?!
    refType { Special $$ "&" } -- For types like &a
    maybeProof { Operator $$ "?" } -- For types like a?
    fromVT { Operator $$ "?!" } -- For types like a?!
    openExistential { Operator $$ "#[" } -- Same as `[` in ATS2
    cblock { CBlockLex _ $$ }
    define { MacroBlock _ $$ }
    lineComment { $$@CommentLex{} }
    lspecial { SpecialBracket $$ }
    atbrace { Special $$ "@{" }
    macdef { Keyword $$ KwMacdef }
    mod { Keyword $$ KwMod }
    fixAt { Keyword $$ KwFixAt }
    lamAt { Keyword $$ KwLambdaAt }
    llamAt { Keyword $$ KwLinearLambdaAt }
    infixr { FixityTok $$ "infixr" }
    infixl { FixityTok $$ "infixl" }
    prefix { FixityTok $$ "prefix" }
    postfix { FixityTok $$ "postfix" }
    customOperator { $$@Operator{} }
    beginComment { CommentBegin $$ }
    endComment { CommentEnd $$ }
    commentContents { CommentContents _ $$ }

%%

some(p)
    : some(p) p { $2 : $1 }
    | p { [$1] }

many(p)
    : some(p) p { $2 : $1 }
    | { [] }

sep_by(p,sep) : p { $1 :| [] }
              | sep_by(p,sep) sep p { $3 :| (toList $1) }

comma_sep(p)
    : sep_by(p, comma) { $1 }

parens(p) : openParen p closeParen { $2 }

sqbrackets(p) : lsqbracket p rsqbracket { $2 }

braces(p) : lbrace p rbrace { $2 }

alt(p,q) : p { $1 }
         | q { $1 }

optional(p) : p { Just $1 }
            | { Nothing }

ATS :: { ATS AlexPosn }
    : Declarations { ATS (reverse $1) }

-- | Parse declarations in a list
Declarations :: { [Declaration AlexPosn] }
             : { [] }
             | Declarations Declaration { $2 : $1 }
             | Declarations FunDecl { $2 ++ $1 }
             | Declarations ValDecl { $2 ++ $1 }
             | Declarations local ATS in ATS end { Local $2 $3 $5 : $1 }

TypeIn :: { NonEmpty (Type AlexPosn) }
       : comma_sep(Type) { $1 }

ExprType :: { Type AlexPosn }
         : StaticExpression { ConcreteType $1 }

TypeInExpr :: { [Type AlexPosn] }
           : comma_sep(alt(Type,ExprType)) { toList $1 }

-- | Parse a type
Type :: { Type AlexPosn }
     : Name parens(TypeInExpr) { Dependent $1 $2 }
     | Name doubleParens { Dependent $1 [] }
     | identifierSpace parens(TypeInExpr) { Dependent (Unqualified $ to_string $1) $2 }
     | identifierSpace { Named (Unqualified $ to_string $1) }
     | specialIdentifier string { Dependent (SpecialName (token_posn $1) (to_string $1)) [Named (Unqualified $ $2)] }
     | Name { Named $1 }
     | exclamation Type { Unconsumed $2 }
     | Type mutateArrow Type { FunctionType "->" $1 $3 }
     | Type funcArrow Type { FunctionType $2 $1 $3 }
     | refType Type { RefType $2 }
     | Type maybeProof { MaybeVal $1 }
     | Name maybeProof { MaybeVal (Named $1) }
     | Type fromVT { FromVT $1 }
     | Type prfTransform Type { AsProof $1 (Just $3) }
     | view at Type { ViewType $1 $3 }
     | viewPlusMinus { ViewLiteral $1 }
     | Existential optional(Type) { Ex $1 $2 }
     | Universal Type { ForA $1 $2 }
     | Type at StaticExpression { AtExpr $2 $1 $3 }
     | at sqbrackets(Type) sqbrackets(StaticExpression) { ArrayType $1 $2 $3 }
     | atbrace Records rbrace { AnonymousRecord $1 $2 }
     | openParen TypeIn vbar TypeIn closeParen { ProofType $1 $2 $4 }
     | identifierSpace identifier { Dependent (Unqualified $ to_string $1) [Named (Unqualified $ to_string $2)] }
     | openParen TypeIn closeParen { Tuple $1 (toList $2) }
     | openParen TypeIn closeParen lineComment { Tuple $1 (toList $2) }
     | boxTuple TypeIn closeParen { BoxTuple $1 $2 }
     | boxTuple TypeIn closeParen lineComment { BoxTuple $1 $2 }
     | addr { AddrType $1 }
     | doubleParens { Tuple $1 mempty }
     | Type where IdentifierOr SortArgs eq Type { WhereType $2 $1 $3 $4 $6 }
     | openParen TypeIn rbrace {% left $ Expected $3 ")" "}" }
     | dollar {% left $ Expected $1 "Type" "$" }
     | identifierSpace identifier openParen {% left $ Expected (token_posn $2) "Static integer expression" (to_string $2) }
     | Type identifierSpace {% left $ Expected (token_posn $2) "," (to_string $2) }

-- | A comma-separated list of arguments
Args :: { [Arg AlexPosn] }
     : Arg { [$1] }
     | Args comma Arg { $3 : $1 }
     | Args vbar Arg { [ PrfArg $1 $3 ] }
     | lineComment { [] }
     | Args lineComment { $1 }
     | Args Comment { $1 }
     | { [] }

TypeArg :: { Arg AlexPosn }
        : IdentifierOr { Arg (First $1) }
        | IdentifierOr colon Type { Arg (Both $1 $3) }
        | Type { Arg (Second $1) }
        | exclamation IdentifierOr colon {% left $ OneOf $3 [",", ")"] ":" }

Arg :: { Arg AlexPosn }
    : TypeArg { $1 }
    | StaticExpression { Arg (Second (ConcreteType $1)) } -- TODO: have some sort of state showing bound variables that we can use to disambiguate types vs. static expressions?

-- | Parse a literal
Literal :: { Expression AlexPosn }
        : uintLit { UintLit $1 }
        | intLit { IntLit $1 }
        | hexLit { HexLit $1 }
        | floatLit { FloatLit $1 }
        | string { StringLit $1 }
        | charLit { CharLit $1 }
        | doubleParens { VoidLiteral $1 }

-- | Parse a list of comma-separated patterns
PatternIn :: { [Pattern AlexPosn] }
          : comma_sep(Pattern) { reverse (toList $1) }

-- | Parse a pattern match
Pattern :: { Pattern AlexPosn }
        : Name { PName $1 [] }
        | identifierSpace { PName (Unqualified $ to_string $1) [] }
        | identifier doubleParens { PName (Unqualified (to_string $1 ++ "()")) [] }
        | tilde Pattern { Free $2 }
        | Name openParen PatternIn closeParen { PName $1 $3 }
        | identifier Pattern { PSum (to_string $1) $2 }
        | identifierSpace Pattern { PSum (to_string $1) $2 }
        | openParen PatternIn vbar PatternIn closeParen { Proof $1 $2 $4 }
        | boxTuple PatternIn closeParen { BoxTuplePattern $1 $2 }
        | parens(PatternIn) { TuplePattern $1 }
        | Literal { PLiteral $1 }
        | Pattern when Expression { Guarded $2 $3 $1 }
        | at Pattern { AtPattern $1 $2 }
        | identifier Universals Pattern { UniversalPattern (token_posn $1) (to_string $1) $2 $3 }
        | identifierSpace Universals Pattern { UniversalPattern (token_posn $1) (to_string $1) $2 $3 }
        | Pattern customOperator Pattern { BinPattern (token_posn $2) (SpecialInfix (token_posn $2) (to_string $2)) $1 $3 }
        | Pattern as Pattern { As $2 $1 $3 }
        | Existential Pattern { ExistentialPattern $1 $2 }
        | minus {% left $ Expected $1 "Pattern" "-" }
        | plus {% left $ Expected $1 "Pattern" "+" }

-- | Common parser for @case@ expressions, @ifcase@ expressions, and @case@
-- static expressions.
case_pattern(pat,expr)
    : vbar pat CaseArrow expr { [($2, $3, $4)] }
    | pat CaseArrow expr { [($1, $2, $3)] }
    | case_pattern(pat, expr) vbar pat CaseArrow expr { ($3, $4, $5) : $1 }

Case :: { [(Pattern AlexPosn, LambdaType AlexPosn, Expression AlexPosn)] }
     : case_pattern(Pattern, Expression) { $1 }

StaticCase :: { [(Pattern AlexPosn, LambdaType AlexPosn, StaticExpression AlexPosn)] }
           : case_pattern(Pattern, StaticExpression) { $1 }

IfCase :: { [(Expression AlexPosn, LambdaType AlexPosn, Expression AlexPosn)] }
       : case_pattern(Expression, Expression) { $1 }

ExpressionPrf :: { (Maybe [Expression AlexPosn], [Expression AlexPosn]) }
              : ExpressionIn { (Nothing, $1) }
              | ExpressionIn vbar ExpressionIn { (Just $1, $3) }

-- | A list of comma-separated expressions
ExpressionIn :: { [Expression AlexPosn] }
             : comma_sep(Expression) { toList $1 }

Tuple :: { NonEmpty (Expression AlexPosn) }
      : PreExpression comma PreExpression { $3 :| [$1] }
      | Tuple comma PreExpression { $3 :| toList $1 }

-- | Parse an arrow in a case statement
CaseArrow :: { LambdaType AlexPosn }
          : plainArrow { Plain $1 }
          | spear { Spear $1 }
          | proofArrow { ProofArrow $1 }
          | proofSpear { ProofSpear $1 }
          | minus {% left $ Expected $1 "Arrow" "-" }
          | eq {% left $ Expected $1 "Arrow" "=" }
          | minus {% left $ Expected $1 "Arrow" "-" }
          | CaseArrow vbar {% left $ Expected $2 "Expression" "|" }

LambdaArrow :: { LambdaType AlexPosn }
            : plainArrow { Plain $1 }
            | cloref1Arrow { Full $1 "cloref1" } -- FIXME this is a bad heuristic
            | cloptr1Arrow { Full $1 "cloptr1" }
            | lincloptr1Arrow { Full $1 "lincloptr1" }
            | lincloref1Arrow { Full $1 "lin,cloref1" }
            | proofLambda { Full $1 "prf" }
            | proofLinearLambda { Full $1 "lin,prf" }
            | minus {% left $ Expected $1 "Arrow" "-" }
            | openParen {% left $ Expected $1 "Arrow" "(" }
            | closeParen {% left $ Expected $1 "Arrow" ")" }

-- | Expression or named call to an expression
Expression :: { Expression AlexPosn }
           : identifierSpace PreExpression { Call (Unqualified $ to_string $1) [] [] Nothing [$2] }
           | PreExpression { $1 }
           | openParen PreExpression comma PreExpression vbar PreExpression closeParen { ProofExpr $1 ($2 :| [$4]) $6 }
           | openParen comma_sep(PreExpression) vbar PreExpression closeParen { ProofExpr $1 $2 $4 }
           | Expression semicolon Expression { Precede $1 $3 }
           | Expression semicolon { $1 }
           | openParen Expression closeParen { $2 }
           | Expression colon Type { TypeSignature $1 $3 }
           | list_vt braces(Type) parens(ExpressionIn) { ListLiteral $1 "vt" $2 $3 }
           | begin Expression extern {% left $ Expected $3 "end" "extern" }
           | Expression prfTransform underscore {% left $ Expected $2 "Rest of expression or declaration" ">>" }

TypeArgs :: { [[Type AlexPosn]] }
         : braces(alt(Type,ExprType)) { [[$1]] }
         | braces(TypeInExpr) { [$1] }
         | TypeArgs braces(alt(Type,ExprType)) { [$2] : $1 }
         | braces(doubleDot) { [[ ImplicitType $1 ]] } -- FIXME only valid on function calls
         | TypeArgs braces(TypeInExpr) { [$2] ++ $1 }

Call :: { Expression AlexPosn }
     : Name doubleParens { Call $1 [] [] Nothing [] }
     | Name parens(ExpressionPrf) { Call $1 [] [] (fst $2) (snd $2) }
     | identifierSpace parens(ExpressionPrf) { Call (Unqualified $ to_string $1) [] [] (fst $2) (snd $2) }
     | Name TypeArgs parens(ExpressionPrf) { Call $1 [] $2 (fst $3) (snd $3) }
     | Name TypeArgs doubleParens { Call $1 [] $2 Nothing [VoidLiteral $3] }
     | Name TypeArgs { Call $1 [] $2 Nothing [] }
     | Name Implicits doubleParens { Call $1 $2 [] Nothing [VoidLiteral $3] }
     | Name Implicits parens(ExpressionPrf) { Call $1 $2 [] (fst $3) (snd $3) }
     | Name Implicits { Call $1 $2 [] Nothing [] }
     | raise PreExpression { Call (SpecialName $1 "raise") [] [] Nothing [$2] } -- $raise can have at most one argument
     | Name Implicits openParen ExpressionPrf fun {% left $ Expected $5 ")" "fun" }
     | Name openParen ExpressionPrf end {% left $ Expected $4 ")" "end"}
     | Name openParen ExpressionPrf else {% left $ Expected $4 ")" "else"}

StaticArgs :: { [StaticExpression AlexPosn] }
           : comma_sep(StaticExpression) { reverse (toList $1) }

StaticDecls :: { [Declaration AlexPosn] }
            : StaticDeclaration { [$1] }
            | StaticDecls StaticDeclaration { $2 : $1 }
            | StaticDecls FunDecl { $2 ++ $1 }
            | FunDecl StaticDecls { $1 ++ $2 }

StaticExpression :: { StaticExpression AlexPosn }
                 : Name { StaticVal $1 }
                 | StaticExpression BinOp StaticExpression { StaticBinary $2 $1 $3 }
                 | intLit { StaticInt $1 }
                 | hexLit { StaticHex $1 }
                 | string { SString $1 }
                 | doubleParens { StaticVoid $1 } -- TODO: static tuple?
                 | sif StaticExpression then StaticExpression else StaticExpression { Sif $2 $4 $6 }
                 | identifierSpace { StaticVal (Unqualified $ to_string $1) }
                 | identifierSpace StaticExpression { SCall (Unqualified $ to_string $1) [] [] [$2] }
                 | Name parens(StaticArgs) { SCall $1 [] [] $2 }
                 | Name Implicits parens(StaticArgs) { SCall $1 $2 [] $3 }
                 | Name TypeArgs parens(StaticArgs) { SCall $1 [] $2 $3 }
                 | Name TypeArgs doubleParens { SCall $1 [] $2 [] }
                 | Name doubleParens { SCall $1 [] [] [] }
                 | identifierSpace TypeArgs parens(StaticArgs) { SCall (Unqualified $ to_string $1) [] $2 $3 }
                 | identifierSpace parens(StaticArgs) { SCall (Unqualified $ to_string $1) [] [] $2 }
                 | identifierSpace doubleParens { SCall (Unqualified $ to_string $1) [] [] [] } -- TODO: this causes an ambiguity because we might have SCall void instead!
                 | StaticExpression semicolon StaticExpression { SPrecede $1 $3 }
                 | UnOp StaticExpression { SUnary $1 $2 }
                 | let StaticDecls comment_after(in) end { SLet $1 (reverse $2) Nothing }
                 | let StaticDecls in StaticExpression end { SLet $1 (reverse $2) (Just $4) }
                 | parens(StaticExpression) { $1 }
                 | openParen lineComment StaticExpression closeParen { $3 }
                 | case StaticExpression of StaticCase { SCase $1 $2 $4 }
                 | openExistential StaticExpression vbar StaticExpression rsqbracket { Witness $1 $2 $4 }
                 | lambda Pattern LambdaArrow StaticExpression { ProofLambda $1 $3 $2 $4 }
                 | llambda Pattern LambdaArrow StaticExpression { ProofLinearLambda $1 $3 $2 $4 }
                 | StaticExpression where braces(ATS) { WhereStaExp $1 $3 }

-- | Parse an expression that can be called without parentheses
PreExpression :: { Expression AlexPosn }
              : identifier lsqbracket PreExpression rsqbracket { Index $2 (Unqualified $ to_string $1) $3 }
              | Literal { $1 }
              | Call { $1 }
              | openParen Tuple closeParen { TupleEx $1 $2 }
              | boxTuple Tuple closeParen { BoxTupleEx $1 $2 }
              | case Expression of Case { Case $3 $1 $2 $4 }
              | ifcase IfCase { IfCase $1 $2 }
              | openParen Expression closeParen { ParenExpr $1 $2 }
              | PreExpression BinOp PreExpression { Binary $2 $1 $3 }
              | UnOp PreExpression { Unary $1 $2 }
              | PreExpression dot Name { Access $2 $1 $3 }
              | PreExpression dot intLit { Access $2 $1 (Unqualified $ show $3) }
              | PreExpression dot identifierSpace { Access $2 $1 (Unqualified $ to_string $3) }
              | if Expression then Expression { If $2 $4 Nothing}
              | if Expression then Expression else Expression { If $2 $4 (Just $6) }
              | let ATS comment_after(in) end { Let $1 $2 Nothing }
              | let ATS in Expression end { Let $1 $2 (Just $4) }
              | let ATS in Expression vbar {% left $ Expected $5 "end" "|" }
              | let ATS fun {% left $ Expected $3 "in" "fun" }
              | let ATS plainArrow {% left $ Expected $3 "in" "=>" }
              | lambda Pattern LambdaArrow Expression { Lambda $1 $3 $2 $4 }
              | llambda Pattern LambdaArrow Expression { LinearLambda $1 $3 $2 $4 }
              | addrAt PreExpression { AddrAt $1 $2 }
              | viewAt PreExpression { ViewAt $1 $2 }
              | atbrace RecordVal rbrace { RecordValue $1 $2 }
              | boxRecord RecordVal rbrace { RecordValue $1 $2 }
              | begin Expression end { Begin $1 $2 }
              | identifierSpace { NamedVal (Unqualified $ to_string $1) }
              | Name { NamedVal $1 }
              | braces(ATS) { Actions $1 }
              | while parens(PreExpression) PreExpression { While $1 $2 $3 }
              | for parens(PreExpression) PreExpression { For $1 $2 $3 }
              | whileStar Universals Termetric parens(Args) plainArrow Expression Expression { WhileStar $1 $2 (snd $3) $4 $6 $7 Nothing }
              | whileStar Universals Termetric parens(Args) colon openParen Args closeParen plainArrow Expression Expression { WhileStar $1 $2 (snd $3) $4 $10 $11 (Just $7) }
              | forStar Universals Termetric parens(Args) plainArrow Expression Expression { ForStar $1 $2 (snd $3) $4 $6 $7 }
              | lineComment PreExpression { CommentExpr (to_string $1) $2 }
              | comma parens(identifier) { MacroVar $1 (to_string $2) }
              | PreExpression where braces(ATS) { WhereExp $1 $3 }
              | include {% left $ Expected $1 "Expression" "include" }
              | staload {% left $ Expected (token_posn $1) "Expression" "staload" }
              | overload {% left $ Expected $1 "Expression" "overload" }
              | var {% left $ Expected $1 "Expression" "var" }
              | Termetric {% left $ Expected (fst $1) "Expression" "termetric" }
              | fromVT {% left $ Expected $1 "Expression" "?!" }
              | prfTransform {% left $ Expected $1 "Expression" ">>" }
              | maybeProof {% left $ Expected $1 "Expression" "?" }
              | let openParen {% left $ Expected $1 "Expression" "let (" }
              | let ATS in Expression lineComment {% left $ Expected (token_posn $5) "end" (take 2 $ to_string $5) }
              | let ATS in Expression extern {% left $ Expected $5 "end" "extern" }
              | let ATS in Expression else {% left $ Expected $5 "end" "else" }
              | let ATS in Expression fun {% left $ Expected $5 "end" "fun" }
              | let ATS in Expression vtypedef {% left $ Expected $5 "end" "vtypedef" }
              | let ATS in Expression implement {% left $ Expected $5 "end" "implement" }
              | let ATS in Expression semicolon {% left $ Expected $5 "end" ";" }
              | let ATS if {% left $ Expected $3 "in" "if" }
              | if Expression then Expression else else {% left $ Expected $6 "Expression" "else" }
              | begin Expression implement {% left $ Expected $3 "end" "implement" }

-- | Parse a termetric
Termetric :: { (AlexPosn, StaticExpression AlexPosn) }
          : openTermetric StaticExpression closeTermetric { ($1, $2) }
          | underscore {% left $ Expected $1 "_" "Termination metric" }
          | dollar {% left $ Expected $1 "$" "Termination metric" }

Sort :: { Sort AlexPosn }
     : t0pPlain { T0p None }
     | t0pCo { T0p Plus }
     | vt0pPlain { Vt0p None }
     | vt0pCo { Vt0p Plus }
     | addr { Addr }
     | view { View $1 None }
     | viewContra { View $1 Minus }
     | viewCo { View $1 Plus }
     | vtype { VType (token_posn $1) (get_addendum $1) }
     | openParen Sort comma Sort closeParen { TupleSort $1 $2 $4 }
     | doubleParens { NamedSort "()" }
     | Sort mutateArrow Sort { ArrowSort $2 $1 $3 }
     | IdentifierOr { NamedSort $1 }
     | IdentifierOr plus { NamedSort ($1 <> "+") }

QuantifierArgs :: { [String] }
               : comma_sep(IdentifierOr) { reverse (toList $1) }
               | { [] }

-- FIXME handle [l:addr;n:int]
Existential :: { Existential AlexPosn }
            : lsqbracket QuantifierArgs colon Sort vbar StaticExpression rsqbracket { Existential $2 False (Just $4) (Just $6) }
            | lsqbracket QuantifierArgs colon Sort rsqbracket { Existential $2 False (Just $4) Nothing }
            | openExistential QuantifierArgs colon Sort rsqbracket { Existential $2 True (Just $4) Nothing }
            | openExistential QuantifierArgs colon Sort vbar StaticExpression rsqbracket { Existential $2 True (Just $4) (Just $6) }
            | sqbrackets(StaticExpression) { Existential mempty False Nothing (Just $1) }

Predicates :: { [StaticExpression AlexPosn] }
           : { [] }
           | StaticExpression { [$1] }
           | Predicates semicolon StaticExpression { $3 : $1 }

-- | Parse a universal quantifier on a type
Universal :: { Universal AlexPosn }
          : lbrace QuantifierArgs rbrace { Universal $2 Nothing [] }
          | lbrace QuantifierArgs vbar Predicates rbrace { Universal $2 Nothing $4 }
          | lbrace QuantifierArgs colon Sort rbrace { Universal $2 (Just $4) [] }
          | lbrace QuantifierArgs colon Sort vbar Predicates rbrace { Universal $2 (Just $4) $6 }

Implicits :: { [[Type AlexPosn]] }
          : lspecial TypeIn rbracket { [toList $2] }
          | Implicits lspecial TypeIn rbracket { toList $3 : $1 }
          | doubleBrackets { [[]] }
          | Implicits doubleBrackets { [] : $1 }
          | lbracket TypeIn rbracket { [toList $2] }
          | Implicits lspecial TypeIn rbracket { toList $3 : $1 }

MaybeImplicit :: { [[Type AlexPosn]] }
              : Implicits { $1 }
              | { [] }

comment_after(p) : p { $1 }
                 | p Comment { $1 }
                 | p lineComment { $1 }

-- | Parse the details of an implementation
Implementation :: { Implementation AlexPosn }
               : comment_after(Universals) FunName MaybeImplicit Universals FunArgs eq Expression { Implement $6 $1 $3 $4 $2 $5 (Right $7) }

StaticImplementation :: { Implementation AlexPosn }
                     : Universals FunName MaybeImplicit Universals FunArgs eq StaticExpression { Implement $6 $1 $3 $4 $2 $5 (Left $7) }

-- | Parse a function name
FunName :: { Name AlexPosn }
        : IdentifierOr { Unqualified $1 }
        | identifier dollar identifier { Functorial (to_string $1) (to_string $3) }
        | dollar identifier dot IdentifierOr { Qualified $1 (to_string $2) $4 }
        | FunName lineComment { $1 }

-- | Parse a general name
Name :: { Name AlexPosn }
     : identifier { Unqualified (to_string $1) }
     | underscore { Unqualified "_" }
     | foldAt { Unqualified "fold@" }
     | identifier dollar IdentifierOr { Unqualified (to_string $1 ++ ('$' : $3)) }
     | identifier dot identifier { FieldName (token_posn $1) (to_string $1) (to_string $3) }
     | identifier dot identifierSpace { FieldName (token_posn $1) (to_string $1) (to_string $3) }
     | identifier dot intLit { FieldName (token_posn $1) (to_string $1) (show $3) }
     | dollar identifier dot identifier { Qualified $1 (to_string $4) (to_string $2) }
     | dollar identifier dot identifierSpace { Qualified $1 (to_string $4) (to_string $2) }
     | dollar identifier dot identifier dollar IdentifierOr { Qualified $1 (to_string $4 ++ ('$' : $6)) (to_string $2) }
     | specialIdentifier { SpecialName (token_posn $1) (to_string $1) }
     | dollar {% left $ Expected $1 "Name" "$" }

-- | Parse a list of values in a record
RecordVal :: { NonEmpty (String, Expression AlexPosn) }
          : IdentifierOr eq Expression { ($1, $3) :| [] }
          | RecordVal comma IdentifierOr eq Expression { ($3, $5) :| toList $1 }
          | IdentifierOr eq comma {% left $ Expected $3 "Expression" "," }
          | identifierSpace {% left $ Expected (token_posn $1) "Record field assignment" (to_string $1) }

-- | Parse a list of types in a record
Records :: { NonEmpty (String, Type AlexPosn) }
        : IdentifierOr eq Type { ($1, $3) :| [] }
        | Records comma IdentifierOr eq Type { ($3, $5) :| toList $1 }

IdentifiersIn :: { [String] }
              : comma_sep(IdentifierOr) { toList $1 }

OfType :: { Maybe (Type AlexPosn) }
       : { Nothing }
       | of Type { Just $2 }

StaticExpressionsIn :: { [StaticExpression AlexPosn] }
                    : comma_sep(StaticExpression) { toList $1 }

-- | Parse a constructor for a sum type
SumLeaf :: { Leaf AlexPosn }
        : vbar Universals identifier { Leaf $2 (to_string $3) [] Nothing }
        | vbar Universals identifierSpace of Type { Leaf $2 (to_string $3) [] (Just $5) }
        | vbar Universals IdentifierOr openParen StaticExpressionsIn closeParen OfType { Leaf $2 $3 $5 $7 } -- FIXME could also be e.g. '0' (static expression)

-- | Parse all constructors of a sum type
Leaves :: { NonEmpty (Leaf AlexPosn) }
       : SumLeaf { $1 :| [] }
       | Leaves SumLeaf { $2 :| toList $1 }
       | Universals identifierSpace of Type { Leaf $1 (to_string $2) [] (Just $4) :| [] }
       | Universals identifier { Leaf $1 (to_string $2) [] Nothing :| [] }
       | Universals identifier openParen StaticExpressionsIn closeParen OfType { Leaf $1 (to_string $2) $4 $6 :| [] }
       | dollar {% left $ Expected $1 "|" "$" }

PreUniversals :: { [Universal AlexPosn] }
              : { [] }
              | doubleBraces { [] }
              | PreUniversals Universal { $2 : $1 }

Universals :: { [Universal AlexPosn] }
           : PreUniversals { reverse $1 }

-- | Optionally parse a termetric
OptTermetric :: { Maybe (StaticExpression AlexPosn) }
             : { Nothing }
             | Termetric { Just (snd $1) }

-- | Parse a unary operator
UnOp :: { UnOp AlexPosn }
UnOp : tilde { Negate }
     | exclamation { Deref }
     | customOperator { SpecialOp (token_posn $1) (to_string $1) }

-- | Parse a binary operator
BinOp :: { BinOp AlexPosn }
      : plus { Add }
      | minus { Sub }
      | div { Div }
      | mult { Mult }
      | geq { GreaterThanEq }
      | leq { LessThanEq }
      | lbracket { LessThan }
      | rbracket { GreaterThan }
      | neq { NotEq }
      | andOp { LogicalAnd }
      | or { LogicalOr }
      | doubleEq { StaticEq }
      | eq { Equal }
      | mod { Mod }
      | percent { Mod }
      | doubleBrackets { NotEq }
      | mutateEq { Mutate }
      | at { At }
      | mutateArrow { SpearOp }
      | customOperator { SpecialInfix (token_posn $1) (to_string $1) }
      | backslash identifierSpace { SpecialInfix $1 ('\\' : to_string $2) }

-- | Optionally parse a function body
OptExpression :: { Maybe (Expression AlexPosn) }
              : { Nothing }
              | eq Expression { Just $2 }
              | let {% left $ Expected $1 "=" "let" }
              | ifcase {% left $ Expected $1 "=" "ifcase" }
              | eq fun {% left $ Expected $2 "Expression" "=" }
              | eq fn {% left $ Expected $2 "Expression" "=" }
              | eq lineComment fun {% left $ Expected $3 "Expression" "=" }
              | lbrace {% left $ Expected $1 "Expression" "{" }

OptStaticExpression :: { Maybe (StaticExpression AlexPosn) }
                    : { Nothing }
                    | eq StaticExpression { Just $2 }

-- | Parse a constructor for a 'dataprop'
DataPropLeaf :: { DataPropLeaf AlexPosn }
             : vbar Universals Expression { DataPropLeaf $2 $3 Nothing }
             | Universals Expression { DataPropLeaf $1 $2 Nothing }
             | vbar Universals Expression of Expression { DataPropLeaf $2 $3 (Just $5) }
             | Universals Expression of Expression { DataPropLeaf $1 $2 (Just $4) }

-- | Parse several constructors for a 'dataprop'
DataPropLeaves :: { [DataPropLeaf AlexPosn] }
               : DataPropLeaf { [$1] }
               | DataPropLeaves DataPropLeaf { $2 : $1 }
               | lineComment { [] }
               | DataPropLeaves lineComment { $1 }
               | prval {% left $ Expected $1 "Constructor" "prval" }
               | var {% left $ Expected $1 "Constructor" "var" }
               | val {% left $ Expected (token_posn $1) "Constructor" "val" }
               | lambda {% left $ Expected $1 "Constructor" "lam" }
               | llambda {% left $ Expected $1 "Constructor" "llam" }
               | minus {% left $ Expected $1 "Constructor" "-" }
               | dollar {% left $ Expected $1 "Constructor" "$" }
               | fromVT {% left $ Expected $1 "Constructor" "?!" }
               | prfTransform {% left $ Expected $1 "Constructor" ">>" }
               | maybeProof {% left $ Expected $1 "Constructor" "?" }

Signature :: { String }
          : signature { $1 }
          | colon { "" }

OptType :: { Maybe (String, Type AlexPosn) }
        : Signature Type { Just ($1, $2) }
        | { Nothing }

-- | Parse a type signature and optional function body
preFunction(p)
    : FunName openParen Args closeParen OptType p { PreF $1 (fmap fst $5) [] [] (Just $3) (fmap snd $5) Nothing $6 }
    | FunName Universals OptTermetric OptType p { PreF $1 (fmap fst $4) [] $2 Nothing (fmap snd $4) $3 $5 }
    | FunName Universals OptTermetric doubleParens OptType p { PreF $1 (fmap fst $5) [] $2 (Just []) (fmap snd $5) $3 $6 }
    | FunName Universals OptTermetric openParen Args closeParen OptType p { PreF $1 (fmap fst $7) [] $2 (Just $5) (fmap snd $7) $3 $8 }
    | Universals FunName Universals OptTermetric openParen Args closeParen OptType p { PreF $2 (fmap fst $8) $1 $3 (Just $6) (fmap snd $8) $4 $9 }
    | Universals FunName Universals OptTermetric doubleParens OptType p { PreF $2 (fmap fst $6) $1 $3 (Just []) (fmap snd $6) $4 $7 }
    | Universals FunName Universals OptTermetric OptType p { PreF $2 (fmap fst $5) $1 $3 Nothing (fmap snd $5) $4 $6 }
    | prval {% left $ Expected $1 "Function signature" "prval" }
    | var {% left $ Expected $1 "Function signature" "var" }
    | val {% left $ Expected (token_posn $1) "Function signature" "val" }
    | lambda {% left $ Expected $1 "Function signature" "lam" }
    | llambda {% left $ Expected $1 "Function signature" "llam" }
    | lsqbracket {% left $ Expected $1 "Function signature" "[" }

PreStaFunction :: { PreFunction StaticExpression AlexPosn } 
               : preFunction(OptStaticExpression) { $1 }

PreFunction :: { PreFunction Expression AlexPosn } 
            : preFunction(OptExpression) { $1 }

-- | Parse affiliated `sortdef`s
AndSort :: { Declaration AlexPosn }
        : AndSort and IdentifierOr eq Sort { AndD $1 (SortDef $2 $3 (Left $5)) }
        | sortdef IdentifierOr eq Sort { SortDef $1 $2 (Left $4) }
        | sortdef IdentifierOr eq Universal { SortDef $1 $2 (Right $4) }

StaticDef :: { Either (StaticExpression AlexPosn, Maybe (Sort AlexPosn)) (Maybe (Type AlexPosn), Type AlexPosn) }
          : eq Type { Right (Nothing, $2) }
          | eq StaticExpression MaybeAnnot { Left ($2, $3) }
          | colon Type eq StaticExpression { Right (Just $2, ConcreteType $4) } -- FIXME wrong wrong bad!!

MaybeAnnot :: { Maybe (Sort AlexPosn) }
           : colon Sort { Just $2 }
           | { Nothing }

AndStadef :: { Declaration AlexPosn }
          : stadef IdentifierOr SortArgs StaticDef { Stadef $2 $3 $4 }
          | stadef IdentifierOr lineComment SortArgs StaticDef { Stadef $2 $4 $5 }
          | stadef Operator SortArgs StaticDef { Stadef $2 $3 $4 }
          | AndStadef and IdentifierOr SortArgs StaticDef { AndD $1 (Stadef $3 $4 $5) }
          | AndStadef and Operator SortArgs StaticDef { AndD $1 (Stadef $3 $4 $5) }

StafunDecl :: { Declaration AlexPosn } 
           : prfun PreStaFunction { Func $1 (PrFun $2) }
           | prfn PreStaFunction { Func $1 (PrFn $2) }

-- | Function declaration
FunDecl :: { [Declaration AlexPosn] }
        : fun PreFunction { [ Func $1 (Fun $2) ] }
        | fnx PreFunction { [ Func $1 (Fnx $2) ] }
        | castfn PreFunction { [ Func $1 (CastFn $2) ] }
        | fn PreFunction identifier {% left $ Expected (token_posn $3) "=" (to_string $3) }
        | fn PreFunction { [ Func $1 (Fn $2) ] }
        | FunDecl and PreFunction { Func $2 (And $3) : $1 }
        | StafunDecl { [$1] }
        | extern FunDecl { over _head (Extern $1) $2 }
        | extern fun PreFunction eq {% left $ Expected $1 "Declaration" "Function body" }
        | extern fnx PreFunction eq {% left $ Expected $1 "Declaration" "Function body" }
        | extern praxi PreFunction eq {% left $ Expected $1 "Declaration" "Function body" }
        | extern prfun PreFunction eq {% left $ Expected $1 "Declaration" "Function body" }
        | extern prfn PreFunction eq {% left $ Expected $1 "Declaration" "Function body" }
        | lambda {% left $ Expected $1 "Function declaration" "lam" }
        | llambda {% left $ Expected $1 "Function declaration" "llam" }
        | fun fn {% left $ Expected $2 "Function name" "fn" }
        | fn fun {% left $ Expected $2 "Function name" "fun" }
        | extern FunDecl identifier openParen {% left $ Expected (token_posn $3) "Static integer expression" (to_string $3) }
        | extern identifier {% left $ OneOf (token_posn $2) ["fun", "fn", "prfun", "prfun", "praxi", "castfn", "fnx", "val", "typedef", "vtypedef"] (to_string $2) }
        | extern extern {% left $ OneOf $2 ["fun", "fn", "prfun", "prfun", "praxi", "castfn", "fnx", "val", "typedef", "vtypedef"] "extern" }

IdentifierOr :: { String }
             : identifier { to_string $1 }
             | identifierSpace { to_string $1 }
             | identifier dollar IdentifierOr { to_string $1 ++ ('$' : $3) }

MaybeType :: { Maybe (Type AlexPosn) }
          : eq Type { Just $2 }
          | { Nothing }

FunArgs :: { Maybe [Arg AlexPosn] }
        : { Nothing }
        | openParen Args closeParen { Just $2 }
        | doubleParens { Just [] }

SortArg :: { [SortArg AlexPosn] }
        : IdentifierOr colon Sort { [ SortArg $1 $3 ] }
        | SortArg comma IdentifierOr colon Sort { SortArg $3 $5 : $1 }
        | SortArg comma IdentifierOr { Anonymous (NamedSort $3) : $1 }
        | SortArg comma Sort { Anonymous $3 : $1 }
        | IdentifierOr { [ Anonymous (NamedSort $1) ] }
        | Sort { [Anonymous $1] }
        | SortArg Comment { $1 }

SortArgs :: { Maybe [SortArg AlexPosn] }
         : parens(SortArg) { Just $1 }
         | doubleParens { Just [] }
         | { Nothing }

AndViewtype :: { Declaration AlexPosn }
            : datavtype IdentifierOr SortArgs eq Leaves { SumViewType $2 $3 $5 }
            | datavtype IdentifierOr SortArgs eq lineComment Leaves { SumViewType $2 $3 $6 }
            | datavtype lineComment IdentifierOr SortArgs eq Leaves { SumViewType $3 $4 $6 }
            | datavtype lineComment IdentifierOr SortArgs eq lineComment Leaves { SumViewType $3 $4 $7 }
            | AndViewtype and IdentifierOr SortArgs eq Leaves { AndD $1 (SumViewType $3 $4 $6) }

AndView :: { Declaration AlexPosn }
        : dataview IdentifierOr SortArgs eq Leaves { DataView $1 $2 $3 $5 }
        | dataview IdentifierOr SortArgs eq lineComment Leaves { DataView $1 $2 $3 $6 }
        | AndView and IdentifierOr SortArgs eq Leaves { AndD $1 (DataView $2 $3 $4 $6) }

AndType :: { Declaration AlexPosn }
        : datatype IdentifierOr SortArgs eq Leaves { SumType $2 $3 $5 }
        | datatype lineComment IdentifierOr SortArgs eq Leaves { SumType $3 $4 $6 }
        | datatype lineComment IdentifierOr SortArgs eq lineComment Leaves { SumType $3 $4 $7 }
        | AndType and IdentifierOr SortArgs eq Leaves { AndD $1 (SumType $3 $4 $6) }

-- | Parse a declaration defining a type
TypeDecl :: { Declaration AlexPosn }
TypeDecl : typedef IdentifierOr SortArgs eq Type MaybeAnnot { TypeDef $1 $2 $3 $5 $6 }
         | vtypedef IdentifierOr SortArgs eq Type { ViewTypeDef $1 $2 $3 $5 }
         | extern vtypedef string SortArgs eq Type { Extern $1 $ ViewTypeDef $2 $3 $4 $6 }
         | abst0p IdentifierOr SortArgs MaybeType { AbsT0p $1 $2 $3 $4 }
         | viewdef IdentifierOr SortArgs eq Type { ViewDef $1 $2 $3 $5 }
         | absvt0p IdentifierOr SortArgs eq Type { AbsVT0p $1 $2 $3 (Just $5) }
         | absview IdentifierOr SortArgs MaybeType { AbsView $1 $2 $3 $4 }
         | abstype IdentifierOr SortArgs MaybeType { AbsType $1 $2 $3 $4 }
         | absvtype IdentifierOr SortArgs MaybeType { AbsViewType $1 $2 $3 $4 }
         | dataprop IdentifierOr SortArgs eq DataPropLeaves { DataProp $1 $2 $3 $5 }
         | absprop IdentifierOr parens(Args) { AbsProp $1 $2 $3 }
         | AndSort { $1 }
         | AndStadef { $1 }
         | AndViewtype { $1 }
         | AndView { $1 }
         | AndType { $1 }
         | extern typedef {% left $ Expected $2 "external declaration" "typedef" }
         | vtypedef IdentifierOr SortArgs eq vbar {% left $ Expected $5 "Viewtype" "|" }
         | typedef IdentifierOr SortArgs eq vbar {% left $ Expected $5 "Type" "|" }
         | datavtype IdentifierOr SortArgs vbar {% left $ Expected $4 "=" "|" }
         | datatype IdentifierOr SortArgs vbar {% left $ Expected $4 "=" "|" }
         | dataview IdentifierOr SortArgs vbar {% left $ Expected $4 "=" "|" }
         | dataprop IdentifierOr SortArgs vbar {% left $ Expected $4 "=" "|" }

EitherInt :: { Fix }
          : intLit { Left $1 }
          | parens(Operator) { Right $1 }

Fixity :: { Fixity AlexPosn }
       : infixr EitherInt { RightFix $1 $2 }
       | infixl EitherInt { LeftFix $1 $2 }
       | prefix EitherInt { Pre $1 $2 }
       | postfix EitherInt { Post $1 $2 }

Operator :: { String }
         : identifierSpace { to_string $1 }
         | customOperator { to_string $1 }
         | mutateArrow { "->" }
         | exclamation { "!" }
         | tilde { "~" }
         | mult { "*" }
         | div { "/" }
         | plus { "+" }
         | minus { "-" }
         | or { "||" }
         | andOp { "&&" }
         | lbracket { "<" }
         | rbracket { ">" }
         | leq { "<=" }
         | geq { ">=" }
         | percent { "%" }
         | mod { "mod" }
         | mutateEq { ":=" }
         | doubleEq { "==" }
         | doubleBrackets { "<>" }
         | neq { "!=" }
         | backslash identifierSpace { '\\' : to_string $2 }
         | Operator Comment { $1 }

Operators :: { [String] }
          : Operator { [$1] }
          | Operators Operator { $2 : $1 }
          | Operators identifier { to_string $2 : $1 }

StackFunction :: { StackFunction AlexPosn }
              : parens(Args) Signature Type plainArrow Expression { StackF $2 $1 $3 $5 }

ValDecl :: { [Declaration AlexPosn] }
        : val Pattern colon Type eq PreExpression { [ Val (get_addendum $1) (Just $4) $2 $6 ] }
        | val Pattern eq Expression { [ Val (get_addendum $1) Nothing $2 $4 ] }
        | ValDecl and Pattern eq Expression { AndDecl Nothing $3 $5 : $1 }
        | extern ValDecl { over _head (Extern $1) $2 }
        | val Pattern eq colon {% left $ Expected $4 "Expression" ":" }

StaticDeclaration :: { Declaration AlexPosn }
                  : prval Pattern eq StaticExpression { PrVal $2 (Just $4) Nothing } -- FIXME: prval should use static expressions as well.
                  | prval Pattern colon Type { PrVal $2 Nothing (Just $4) }
                  | prval Pattern colon Type eq StaticExpression { PrVal $2 (Just $6) (Just $4) }
                  | prvar Pattern eq StaticExpression { PrVar $2 (Just $4) Nothing }
                  | prvar Pattern colon Type { PrVar $2 Nothing (Just $4) }
                  | prvar Pattern colon Type eq StaticExpression { PrVar $2 (Just $6) (Just $4) }
                  | praxi PreStaFunction { Func $1 (Praxi $2) }
                  | primplmnt FunArgs StaticImplementation { ProofImpl $2 $3 }
                  | StafunDecl { $1 }
                  | extern StaticDeclaration { Extern $1 $2 }

DataSortLeaf :: { DataSortLeaf AlexPosn }
             : vbar Universals Sort { DataSortLeaf $2 $3 Nothing }
             | vbar Universals Sort of Sort { DataSortLeaf $2 $3 (Just $5) }
             | DataSortLeaf Comment { $1 }

DataSortLeaves :: { NonEmpty (DataSortLeaf AlexPosn) }
               : DataSortLeaf { $1 :| [] }
               | DataSortLeaves DataSortLeaf { $2 :| toList $1 }

CommentContents :: { Declaration AlexPosn }
                : commentContents { Comment $1 }
                | CommentContents commentContents { over comment (<> $2) $1 }

Comment :: { Declaration AlexPosn }
        : beginComment CommentContents endComment { over comment ((<> "*)") . ("(*" <>)) $2 }

Names :: { [Name AlexPosn] }
      : Name { [$1] }
      | identifierSpace { [Unqualified $ to_string $1] }
      | customOperator { [Unqualified $ to_string $1] }
      | Names Name { $2 : $1 }
      | Names identifierSpace { Unqualified (to_string $2) : $1 }
      | Names customOperator { Unqualified (to_string $2) : $1 }

Load :: { (Bool, Token) }
     : staload { (True, $1) }
     | dynload { (False, $1) }

MacroArgs :: { Maybe [String] }
          : doubleParens { Just [] }
          | parens(IdentifiersIn) { Just $1 }
          | { Nothing }

-- | Parse a declaration
Declaration :: { Declaration AlexPosn }
Declaration : include string { Include $2 }
            | define { Define $1 }
            | extvar string eq Expression { ExtVar $1 $2 $4 }
            | define identifierSpace string { Define ($1 ++ " " ++ to_string $2 ++ $3) } -- FIXME better approach?
            | define identifier string { Define ($1 ++ " " ++ to_string $2 ++ $3) } -- FIXME better approach?
            | define identifierSpace intLit { Define ($1 ++ " " ++ to_string $2 ++ " " ++ show $3) }
            | cblock { CBlock $1 }
            | datasort identifierSpace eq DataSortLeaves { DataSort $1 (to_string $2) $4 }
            | macdef IdentifierOr MacroArgs eq Expression { MacDecl $1 $2 $3 $5 }
            | macdef customOperator MacroArgs eq Expression { MacDecl $1 (to_string $2) $3 $5 }
            | lineComment { Comment (to_string $1) }
            | Comment { $1 }
            | Load underscore eq string { Load (fst $1) (get_staload $ snd $1) (Just "_") $4 }
            | Load string { Load (fst $1) (get_staload $ snd $1) Nothing $2 }
            | Load IdentifierOr eq string { Load (fst $1) (get_staload $ snd $1) (Just $2) $4 }
            | Load dollar IdentifierOr { Load (fst $1) (get_staload $ snd $1) Nothing ('$' : $3) }
            | Load dollar IdentifierOr eq string { Load (fst $1) (get_staload $ snd $1) (Just ('$' : $3)) $5 }
            | Load IdentifierOr eq dollar IdentifierOr { Load (fst $1) (get_staload $ snd $1) (Just $2) ('$' : $5) }
            | var Pattern colon Type with PreExpression { Var (Just $4) $2 Nothing (Just $6) } -- FIXME signature is too general.
            | var Pattern colon Type eq PreExpression { Var (Just $4) $2 (Just $6) Nothing }
            | var Pattern eq Expression { Var Nothing $2 (Just $4) Nothing }
            | var Pattern colon Type { Var (Just $4) $2 Nothing Nothing }
            | var Pattern eq fixAt IdentifierOr StackFunction { Var Nothing $2 (Just $ FixAt $4 $5 $6) Nothing }
            | var Pattern eq lamAt StackFunction { Var Nothing $2 (Just $ LambdaAt $4 $5) Nothing }
            | var Pattern eq llamAt StackFunction { Var Nothing $2 (Just $ LinearLambdaAt $4 $5) Nothing }
            | implement FunArgs Implementation { Impl $2 $3 }
            | StaticDeclaration { $1 }
            | overload doubleSqBrackets with IdentifierOr { OverloadIdent $1 "[]" (Unqualified $4) Nothing }
            | overload BinOp with Name { OverloadOp $1 $2 $4 Nothing }
            | overload BinOp with customOperator { OverloadOp $1 $2 (Unqualified $ to_string $4) Nothing }
            | overload BinOp with identifierSpace of intLit { OverloadOp $1 $2 (Unqualified $ to_string $4) (Just $6) }
            | overload identifierSpace with Name { OverloadIdent $1 (to_string $2) $4 Nothing }
            | overload identifierSpace with identifierSpace { OverloadIdent $1 (to_string $2) (Unqualified $ to_string $4) Nothing }
            | overload identifierSpace with identifierSpace of intLit { OverloadIdent $1 (to_string $2) (Unqualified $ to_string $4) (Just $6) }
            | overload tilde with identifier { OverloadIdent $1 "~" (Unqualified $ to_string $4) Nothing } -- FIXME figure out a general solution.
            | overload tilde with identifierSpace of intLit { OverloadIdent $1 "~" (Unqualified $ to_string $4) (Just $6) }
            | overload doubleSqBrackets with identifierSpace of intLit { OverloadIdent $1 "[]" (Unqualified $ to_string $4) (Just $6) }
            | overload dot identifierSpace with Name { OverloadIdent $1 ('.' : (to_string $3)) $5 Nothing }
            | assume identifierSpace SortArgs eq Type { Assume (Unqualified (to_string $2)) $3 $5 }
            | assume Name SortArgs eq Type { Assume $2 $3 $5 }
            | absimpl identifierSpace SortArgs eq Type { AbsImpl $1 (Unqualified (to_string $2)) $3 $5 }
            | absimpl Name SortArgs eq Type { AbsImpl $1 $2 $3 $5 }
            | tkindef IdentifierOr eq string { TKind $1 (Unqualified $2) $4 }
            | TypeDecl { $1 }
            | symintr Names { SymIntr $1 $2 }
            | stacst IdentifierOr colon Type OptStaticExpression { Stacst $1 (Unqualified $2) $4 $5 }
            | propdef IdentifierOr openParen Args closeParen eq Type { PropDef $1 $2 (Just $4) $7 }
            | exception identifierSpace of doubleParens { Exception (to_string $2) (Tuple $4 mempty) }
            | exception identifierSpace of openParen Type closeParen { Exception (to_string $2) (Tuple $4 [$5]) }
            | Fixity Operators {% addSt $1 $2 }
            | val Universals IdentifierOr colon Type { StaVal $2 $3 $5 }
            | lambda {% left $ Expected $1 "Declaration" "lam" }
            | llambda {% left $ Expected $1 "Declaration" "llam" }
            | dollar {% left $ Expected $1 "Declaration" "$" }
            | fromVT {% left $ Expected $1 "Declaration" "?!" }
            | prfTransform {% left $ Expected $1 "Declaration" ">>" }
            | maybeProof {% left $ Expected $1 "Declaration" "?" }
            | identifier {% left $ Expected (token_posn $1) "Declaration" (to_string $1) }
            | identifierSpace {% left $ Expected (token_posn $1) "Declaration" (to_string $1) }
            | where {% left $ Expected $1 "Declaration" "where" }
            | vbar {% left $ Expected $1 "Declaration" "|" }

{

type ParseSt a = StateT (FixityState AlexPosn) (Either ATSError) a

addSt :: (Fixity AlexPosn) -> [String] -> ParseSt (Declaration AlexPosn)
addSt x keys = modify (thread inserts) >> pure (FixityDecl x keys)
    where inserts = flip M.insert x <$> keys

data ATSError = Expected AlexPosn String String
              | OneOf AlexPosn [String] String
              | Unknown Token
              | LexError String
              | Exhausted
              deriving (Eq, Show, Generic, NFData)

unmatched :: AlexPosn -> String -> Doc
unmatched l chr = "unmatched" <+> squotes (text chr) <+> "at" <+> pretty l <> linebreak

-- ors = bear
bear :: Bool -> [Doc] -> Doc
bear _ [] = mempty
bear False [x, y] = x <+> "or" <+> y
bear True [x, y] = x <> ", or" <+> y
bear _ (x:xs) = x <> "," <+> bear True xs

instance Pretty ATSError where
    pretty = (dullred "Error:" <+>) . preErr

preErr (OneOf p ss s) = pretty p <> linebreak <> (indent 2 $ "Unexpected" <+> squotes (text s) <> ", expected one of" <+> bear False (fmap (squotes . text) ss)) <> linebreak
preErr (Expected p s1 s2) = pretty p <> linebreak <> (indent 2 $ "Unexpected" <+> squotes (text s2) <> ", expected:" <+> squotes (text s1)) <> linebreak
preErr (Unknown (Special l ")")) = unmatched l ")"
preErr (Unknown (Special l "}")) = unmatched l "}"
preErr (Unknown (Special l ">")) = unmatched l ">"
preErr (Unknown t) = "unexpected token" <+> squotes (pretty t) <+> "at" <+> pretty (token_posn t) <> linebreak
preErr (LexError s) = dullred "lexing:" <+> text s <> linebreak
preErr Exhausted = "unexpectedly out of input"

left :: ATSError -> ParseSt b
left = lift . Left

parseError :: [Token] -> ParseSt a
parseError [] = left Exhausted
parseError x = left . Unknown . head $ x

}
