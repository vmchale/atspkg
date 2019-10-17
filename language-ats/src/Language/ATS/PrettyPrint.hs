{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Language.ATS.PrettyPrint ( printATS
                                , printATSCustom
                                , printATSFast
                                ) where

import           Control.Composition          hiding ((&))
import           Control.Recursion            (cata)
import           Data.Bool                    (bool)
import           Data.Foldable                (toList)
import           Data.List                    (isPrefixOf)
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NE
import           Language.ATS.Types
import           Lens.Micro
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen hiding (bool)

infixr 5 $$

instance Eq Doc where
    (==) = (==) `on` (($ "") . displayS . renderCompact)


-- | Pretty-print with sensible defaults.
printATS :: Eq a => ATS a -> String
printATS = (<> "\n") . printATSCustom 0.6 120

printATSCustom :: Eq a
               => Float -- ^ Ribbon fraction
               -> Int -- ^ Ribbon width
               -> ATS a -> String
printATSCustom r i x = g mempty
    where g = (displayS . renderSmart r i . pretty) x

-- | Slightly faster pretty-printer without indendation (for code generation).
printATSFast :: Eq a => ATS a -> String
printATSFast x = g mempty
    where g = (displayS . renderCompact . (<> "\n") . pretty) x

instance Pretty (Name a) where
    pretty (Unqualified n)    = text n
    pretty (Qualified _ i n)  = "$" <> text n <> "." <> text i
    pretty (SpecialName _ s)  = "$" <> text s
    pretty (Functorial s s')  = text s <> "$" <> text s'
    pretty (FieldName _ n n') = text n <> "." <> text n'

instance Pretty (LambdaType a) where
    pretty Plain{}      = "=>"
    pretty Spear{}      = "=>>"
    pretty ProofArrow{} = "=/=>"
    pretty ProofSpear{} = "=/=>>"
    pretty (Full _ v)   = "=<" <> text v <> ">"

instance Pretty (BinOp a) where
    pretty Mult               = "*"
    pretty Add                = "+"
    pretty Div                = "/"
    pretty Sub                = "-"
    pretty GreaterThan        = ">"
    pretty LessThan           = "<"
    pretty Equal              = "="
    pretty NotEq              = "!="
    pretty LogicalAnd         = "&&"
    pretty LogicalOr          = "||"
    pretty LessThanEq         = "<="
    pretty GreaterThanEq      = ">="
    pretty StaticEq           = "=="
    pretty Mod                = "%"
    pretty Mutate             = ":="
    pretty SpearOp            = "->"
    pretty At                 = "@"
    pretty (SpecialInfix _ s) = text s

splits :: BinOp a -> Bool
splits Mult       = True
splits Add        = True
splits Div        = True
splits LogicalAnd = True
splits LogicalOr  = True
splits _          = False

startsParens :: Doc -> Bool
startsParens d = f (show d) where
    f ('(':_) = True
    f _       = False

prettySmall :: Doc -> [Doc] -> Doc
prettySmall op es = mconcat (punctuate (" " <> op <> " ") es)

prettyBinary :: Doc -> [Doc] -> Doc
prettyBinary op es
    | length (showFast $ mconcat es) < 80 = prettySmall op es
    | otherwise = prettyLarge op es

prettyLarge :: Doc -> [Doc] -> Doc
prettyLarge _ []      = mempty
prettyLarge op (e:es) = e <$> vsep (fmap (op <+>) es)

lengthAlt :: Doc -> Doc -> Doc
lengthAlt d1 d2
    | length (showFast d2) >= 30 = d1 <$> indent 4 d2
    | otherwise = d1 <+> d2

prettyArgsProof :: (Pretty a) => Maybe [a] -> [Doc] -> Doc
prettyArgsProof (Just e) = prettyArgsG ("(" <> prettyArgsG mempty mempty (fmap pretty e) <+> "| ") ")"
prettyArgsProof Nothing  = prettyArgs

instance Pretty (UnOp a) where
    pretty Negate          = "~"
    pretty Deref           = "!"
    pretty (SpecialOp _ s) = text s

prettyProofExpr :: NonEmpty Doc -> Doc
prettyProofExpr (e:|[]) = e
prettyProofExpr es      = mconcat (punctuate ", " (toList es))

prettyLam :: (Pretty a, Pretty b) => Doc -> a -> b -> Doc -> Doc
prettyLam bind p lt e = let pre = bind <+> pretty p <+> pretty lt in flatAlt (lengthAlt pre e) (pre <+> e)

instance Eq a => Pretty (Expression a) where
    pretty = cata a where
        a (IfF e e' (Just e''))         = "if" <+> e <+> "then" <$> indent 2 e' <$> "else" <$> indent 2 e''
        a (IfF e e' Nothing)            = "if" <+> e <+> "then" <$> indent 2 e'
        a (LetF _ e e')          = flatAlt
            ("let" <$> indent 2 (pretty e) <$> endLet e')
            ("let" <+> pretty e <$> endLet e')
        a (UintLitF u)                  = pretty (fromIntegral u :: Integer) <> "u"
        a (IntLitF i)                   = pretty i
        a (HexLitF hi)                  = "0x" <> text hi
        a (LambdaF _ lt p e)            = prettyLam "lam" p lt e
        a (LinearLambdaF _ lt p e)      = prettyLam "llam" p lt e
        a (FloatLitF f)                 = pretty f
        a (StringLitF s)                = text s -- FIXME escape indentation in multi-line strings.
        a (ParenExprF _ e)              = parens e
        a (UnaryF op e) = pretty op <> pretty e
        a (BinListF op@Add es)          = prettyBinary (pretty op) es
        a (BinListF op@Con{} es)        = prettyBinary (pretty op) es
        a (BinaryF op e e')
            | splits op = e </> pretty op <+> e'
            | otherwise = e <+> pretty op <+> e'
        a (IndexF _ n e)                = pretty n <> brackets e
        a (NamedValF nam)               = pretty nam
        a (CallF nam [] [] Nothing [])  = pretty nam <> "()"
        a (CallF nam [] [] e xs)        = pretty nam <> prettyArgsProof e xs
        a (CallF nam [] us Nothing [])  = pretty nam <> prettyTypes us
        a (CallF nam [] us Nothing ["()"]) = pretty nam <> prettyTypes us <> "()"
        a (CallF nam [] us e xs)        = pretty nam <> prettyTypes us <> prettyArgsProof e xs
        a (CallF nam is [] Nothing [])  = pretty nam <> prettyImplicits is
        a (CallF nam is [] Nothing ["()"]) = pretty nam <> prettyImplicits is <> "()"
        a (CallF nam is [] e xs)        = pretty nam <> prettyImplicits is <> prettyArgsProof e xs
        a (CallF nam is us Nothing ["()"]) = pretty nam <> prettyImplicits is <> prettyTypes us <> "()"
        a (CallF nam is us e xs)        = pretty nam <> prettyImplicits is <> prettyTypes us <> prettyArgsProof e xs
        a (CaseF _ add' e cs)           = "case" <> pretty add' <+> e <+> "of" <$> indent 2 (prettyCases cs)
        a (IfCaseF _ cs)                = "ifcase" <$> indent 2 (prettyIfCase cs)
        a (VoidLiteralF _)              = "()"
        a (RecordValueF _ es)           = prettyRecord es
        a (BoxRecordValueF _ es)        = "'" <> prettyRecord es
        a (PrecedeF e e')               = parens (e <+> ";" </> e')
        a (PrecedeListF es)             = lineAlt (prettyArgsList "; " "(" ")" es) ("(" <> mconcat (punctuate " ; " es) <> ")")
        a (AccessF _ e n)
            | noParens e = e <> "." <> pretty n
            | otherwise  = parens e <> "." <> pretty n
        a (CharLitF '\\')              = "'\\\\'"
        a (CharLitF '\n')              = "'\\n'"
        a (CharLitF '\t')              = "'\\t'"
        a (CharLitF '\0')              = "'\\0'"
        a (CharLitF c)                 = "'" <> char c <> "'"
        a (ProofExprF _ es e')         = "(" <> prettyProofExpr es <+> "|" <+> e' <> ")"
        a (TypeSignatureF e t)         = e <+> ":" <+> pretty t
        a (WhereExpF e d)              = prettyWhere e d
        a (TupleExF _ es)              = parens (mconcat $ punctuate ", " (toList $ NE.reverse es))
        a (BoxTupleExF _ es)           = "'(" <> mconcat (punctuate ", " (toList $ NE.reverse es)) <> ")"
        a (WhileF _ e e')              = "while" <> parens e <> e'
        a (ForF _ e e')                = "for" <> parens e <> e'
        a (WhileStarF _ us t as e e' Nothing)   = "while*" <> prettyUsStarNil us <> prettyTermetric t <> prettyArgs as <+> "=>" <$> indent 4 e <$> indent 4 e'
        a (WhileStarF _ us t as e e' (Just ty)) = "while*" <> prettyUsStarNil us <> prettyTermetric t <> prettyArgs as <+> ":" <+> prettyArgs ty <+> "=>" <$> indent 4 e <$> indent 4 e'
        a (ForStarF _ us t as e e')    = "for*" <> prettyUsStarNil us <> prettyTermetric t <> prettyArgs as <+> "=>" <$> indent 4 e <$> indent 4 e'
        a (ActionsF (ATS [d]))         = "{" <+> pretty d <+> "}"
        a (ActionsF as)                = "{" <$> indent 2 (pretty as) <$> "}"
        a UnderscoreLitF{}             = "_"
        a (BeginF _ e)
            | not (startsParens e) = linebreak <> indent 2 ("begin" <$> indent 2 e <$> "end")
            | otherwise          = e
        a (FixAtF _ n (StackF s as t e))  = "fix@" <+> text n <+> prettyArgs as <+> ":" <> pretty s <+> pretty t <+> "=>" <$> indent 2 (pretty e)
        a (LambdaAtF _ (StackF s as t e)) = "lam@" <+> prettyArgs as <+> ":" <> pretty s <+> pretty t <+> "=>" <$> indent 2 (pretty e)
        a (LinearLambdaAtF _ (StackF s as t e)) = "llam@" <+> prettyArgs as <+> ":" <> pretty s <+> pretty t <+> "=>" <$> indent 2 (pretty e)
        a (AddrAtF _ e)                   = "addr@" <> e
        a (ViewAtF _ e)                   = "view@" <> e
        a (ListLiteralF _ s t es)         = "list" <> string s <> "{" <> pretty t <> "}" <> prettyArgs es
        a (CommentExprF c e) = text c <$> e
        a (MacroVarF _ s) = ",(" <> text s <> ")"
        a BinListF{} = undefined -- Shouldn't happen

        prettyIfCase []              = mempty
        prettyIfCase [(s, l, t)]     = "|" <+> s <+> pretty l <+> t
        prettyIfCase ((s, l, t): xs) = prettyIfCase xs $$ "|" <+> s <+> pretty l <+> t

prettyCases :: (Pretty a, Pretty b) => [(a, b, Doc)] -> Doc
prettyCases []              = mempty
prettyCases [(s, l, t)]     = "|" <+> pretty s <+> pretty l <+> t
prettyCases ((s, l, t): xs) = prettyCases xs $$ "|" <+> pretty s <+> pretty l <+> t -- FIXME can leave space with e.g. => \n begin ...

noParens :: Doc -> Bool
noParens = all (`notElem` ("()" :: String)) . show

patternHelper :: [Doc] -> Doc
patternHelper ps = mconcat (punctuate ", " ps)

instance Eq a => Pretty (Pattern a) where
    pretty = cata a where
        a (PSumF s x)                         = string s <+> x
        a (PLiteralF e)                       = pretty e
        a (PNameF s [])                       = pretty s
        a (PNameF s [x])                      = pretty s <> parens x
        a (PNameF s ps)                       = pretty s <> parens (patternHelper ps)
        a (FreeF p)                           = "~" <> p
        a (GuardedF _ e p)                    = p <+> "when" <+> pretty e
        a (ProofF _ p p')                     = parens (patternHelper p <+> "|" <+> patternHelper p')
        a (TuplePatternF ps)                  = parens (patternHelper ps)
        a (BoxTuplePatternF _ ps)             = "'(" <> patternHelper ps <> ")"
        a (AtPatternF _ p)                    = "@" <> p
        a (UniversalPatternF _ n us (Just p)) = text n <> prettyArgsU "" "" us <> p
        a (UniversalPatternF _ n us Nothing)  = text n <> prettyArgsU "" "" us
        a (ExistentialPatternF e p)           = pretty e <> p
        a (AsF _ p p')                        = p <+> "as" <+> p'
        a (BinPatternF _ op p p')             = p <+> pretty op <+> p'

argHelper :: Eq a => (Doc -> Doc -> Doc) -> Arg a -> Doc
argHelper _ (Arg (First s))   = pretty s
argHelper _ (Arg (Second t))  = pretty t
argHelper op (Arg (Both s t)) = pretty s `op` colon `op` pretty t
argHelper op (PrfArg a a')    = prettyArgs' ", " mempty mempty a </> "|" `op` pretty a'

instance Eq a => Pretty (SortArg a) where
    pretty (SortArg n st) = text n <> ":" <+> pretty st
    pretty (Anonymous s)  = pretty s

instance Eq a => Pretty (Arg a) where
    pretty = argHelper (<+>)

squish :: BinOp a -> Bool
squish Add  = True
squish Sub  = True
squish Mult = True
squish _    = False

endLet :: Maybe Doc -> Doc
endLet Nothing  = "in end"
endLet (Just d) = "in" <$> indent 2 d <$> "end"

prettyExtras :: Pretty a => Doc -> Doc -> [[a]] -> Doc
prettyExtras d1 d2 = foldMap (prettyArgsU d1 d2) . reverse

prettyTypes :: Pretty a => [[a]] -> Doc
prettyTypes = prettyExtras "{" "}"

prettyImplicits :: Pretty a => [[a]] -> Doc
prettyImplicits = prettyExtras "<" ">"

prettyWhere :: Pretty a => Doc -> a -> Doc
prettyWhere e d = e <+> "where" <$> braces (" " <> nest 2 (pretty d) <> " ")

instance Eq a => Pretty (StaticExpression a) where
    pretty = cata a where
        a (StaticValF n)            = pretty n
        a (StaticBinaryF op se se')
            | squish op = se <> pretty op <> se'
            | otherwise = se <+> pretty op <+> se'
        a (StaticIntF i)               = pretty i
        a (StaticHexF h)               = text h
        a StaticVoidF{}                = "()"
        a (SifF e e' e'')              = "sif" <+> e <+> "then" <$> indent 2 e' <$> "else" <$> indent 2 e''
        a (SCallF n [] [] ["()"] Nothing)      = pretty n <> "()"
        a (SCallF n [] us ["()"] Nothing)      = pretty n <> prettyTypes us <> "()"
        a (SCallF n [] [] cs Nothing)          = pretty n <> parens (mconcat (punctuate "," . fmap pretty $ cs))
        a (SCallF n [] us cs Nothing)          = pretty n <> prettyTypes us <> parens (commaTight cs)
        a (SCallF n is [] ["()"] Nothing)      = pretty n <> prettyImplicits is <> "()"
        a (SCallF n is us ["()"] Nothing)      = pretty n <> prettyImplicits is <> prettyTypes us <> "()"
        a (SCallF n is [] cs Nothing)          = pretty n <> prettyImplicits is <> parens (commaTight cs)
        a (SCallF n is us cs Nothing)          = pretty n <> prettyImplicits is <> prettyTypes us <> parens (commaTight cs)
        a (SCallF n [] [] cs (Just ds))        = pretty n <> parens (commaTight cs <+> "|" <+> commaTightDyn ds)
        a (SCallF n [] us cs (Just ds))        = pretty n <> prettyTypes us <> parens (commaTight cs <+> "|" <+> commaTightDyn ds)
        a (SCallF n is [] cs (Just ds))        = pretty n <> prettyImplicits is <> parens (commaTight cs <+> "|" <+> commaTightDyn ds)
        a (SCallF n is us cs (Just ds))        = pretty n <> prettyImplicits is <> prettyTypes us <> parens (commaTight cs <+> "|" <+> commaTightDyn ds)
        a (SPrecedeF e e')             = e <> ";" <+> e'
        a (SPrecedeListF es)           = lineAlt (prettyArgsList "; " "(" ")" es) ("(" <> mconcat (punctuate " ; " es) <> ")")
        a (SParensF e)                 = parens e
        a (SUnaryF op e)               = pretty op <> e
        a (SLetF _ e e') = flatAlt
            ("let" <$> indent 2 (concatSame e) <$> endLet e')
            ("let" <+> concatSame e <$> endLet e')
        a (SCaseF ad e sls) = "case" <> pretty ad <+> e <+> "of" <$> indent 2 (prettyCases sls)
        a (SStringF s)      = text s
        a (WitnessF _ e e') = "#[" <+> e <+> "|" <+> e' <+> "]"
        a (ProofLambdaF _ lt p e)       = prettyLam "lam" p lt e
        a (ProofLinearLambdaF _ lt p e) = prettyLam "llam" p lt e
        a (WhereStaExpF e ds) = prettyWhere e ds

        commaTight :: [Doc] -> Doc
        commaTight = mconcat . punctuate ","

        commaTightDyn :: Pretty b => [b] -> Doc
        commaTightDyn = commaTight . fmap pretty

instance Eq a => Pretty (Sort a) where
    pretty = cata a where
        a (T0pF ad)           = "t@ype" <> pretty ad
        a (Vt0pF ad)          = "vt@ype" <> pretty ad
        a (NamedSortF s)      = text s
        a AddrF               = "addr"
        a (ViewF _ t)         = "view" <> pretty t
        a (VTypeF _ a')       = "vtype" <> pretty a'
        a (TupleSortF _ s s') = parens (s <> "," <+> s')
        a (ArrowSortF _ s s') = s <+> "->" <+> s'

instance Eq a => Pretty (Type a) where
    pretty = cata a where
        a (NamedF n)                       = pretty n
        a (ViewTypeF _ t)                  = "view@" <> parens t
        a (ExF e (Just t))
            | head (show t) == '['         = pretty e <> t -- FIXME this is kinda dumb
            | otherwise                    = pretty e <+> t
        a (ExF e Nothing)                  = pretty e
        a (DependentF n@SpecialName{} [t]) = pretty n <+> pretty t
        a (DependentF n ts)                = pretty n <> parens (mconcat (punctuate ", " (fmap pretty (reverse ts))))
        a (ForAF u t)                      = pretty u <+> t
        a (UnconsumedF t)                  = "!" <> t
        a (AsProofF t (Just t'))           = t <+> ">>" <+> t'
        a (AsProofF t Nothing)             = t <+> ">> _"
        a (FromVTF t)                      = t <> "?!"
        a (MaybeValF t)                    = t <> "?"
        a (AtExprF _ t t')                 = t <+> "@" <+> pretty t'
        a (ArrayTypeF _ t n)               = "@[" <> t <> "][" <> pretty n <> "]"
        a (ProofTypeF _ t t')              = parens (pre' `op` "|" <+> prettyArgsG mempty mempty t')
            where pre' = prettyArgsG mempty mempty t
                  op = if '\n' `elem` showFast pre' then (<>) else (<+>)
        a (ConcreteTypeF e)                = pretty e
        a (TupleF _ ts)                    = parens (mconcat (punctuate ", " (fmap pretty (reverse ts))))
        a (BoxTupleF _ ts)                 = "'(" <> mconcat (punctuate ", " (toList $ fmap pretty (NE.reverse ts))) <> ")"
        a (RefTypeF t)                     = "&" <> t
        a (FunctionTypeF s t t')           = t <+> string s <+> t'
        a (ViewLiteralF c)                 = "view" <> pretty c
        a ImplicitTypeF{}                  = ".."
        a (AnonymousRecordF _ rs)          = prettyRecord rs
        a (WhereTypeF _ t i sa t')         = t <#> indent 2 ("where" </> pretty i <+> prettySortArgs sa <+> "=" <+> pretty t')
        a AddrTypeF{}                      = "addr"

gan :: Eq a => Maybe (Sort a) -> Doc
gan (Just t) = " : " <> pretty t <> " "
gan Nothing  = ""

withHashtag :: Bool -> Doc
withHashtag True = "#["
withHashtag _    = lbracket

instance Eq a => Pretty (Existential a) where
    pretty (Existential [] b (Just st) (Just e')) = withHashtag b <> pretty st <> pretty e' <> rbracket
    pretty (Existential [] b Nothing (Just e'))   = withHashtag b <> pretty e' <> rbracket
    pretty (Existential [e] b (Just st) Nothing)  = withHashtag b <> text e <> ":" <> pretty st <> rbracket
    pretty (Existential bs b st Nothing)          = withHashtag b <+> mconcat (punctuate ", " (fmap pretty bs)) <> gan st <+> rbracket
    pretty (Existential bs b st (Just e))         = withHashtag b <+> mconcat (punctuate ", " (fmap pretty bs)) <> gan st <> "|" <+> pretty e <+> rbracket

instance Eq a => Pretty (Universal a) where
    pretty (Universal [x] Nothing []) = lbrace <> text x <> rbrace
    pretty (Universal [x] (Just st) []) = lbrace <> text x <> ":" <> pretty st <> rbrace
    pretty (Universal bs Nothing []) = lbrace <> mconcat (punctuate "," (fmap pretty bs)) <> rbrace
    pretty (Universal bs (Just ty) []) = lbrace <+> mconcat (punctuate ", " (fmap pretty bs)) <+> ":" <+> pretty ty <+> rbrace
    pretty (Universal bs ty es) = lbrace <+> mconcat (punctuate ", " (fmap pretty bs)) <> gan ty <> "|" <+> mconcat (punctuate "; " (fmap pretty es)) <+> rbrace

instance Eq a => Pretty (ATS a) where
    pretty (ATS xs) = concatSame xs

prettyOr :: (Pretty a, Eq a) => [[a]] -> Doc
prettyOr [] = mempty
prettyOr is = mconcat (fmap (prettyArgsU "<" ">") is)

prettyImplExpr :: Eq a => Either (StaticExpression a) (Expression a) -> Doc
prettyImplExpr (Left se) = pretty se
prettyImplExpr (Right e) = pretty e

instance Eq a => Pretty (Implementation a) where
    pretty (Implement _ [] is [] n (Just []) e)  = pretty n <> prettyOr is <+> "() =" <$> indent 2 (prettyImplExpr e)
    pretty (Implement _ [] is [] n Nothing e)    = pretty n <> prettyOr is <+> "=" <$> indent 2 (prettyImplExpr e)
    pretty (Implement _ [] is [] n (Just ias) e) = pretty n <> prettyOr is <+> prettyArgs ias <+> "=" <$> indent 2 (prettyImplExpr e)
    pretty (Implement _ [] is us n (Just ias) e) = pretty n <> prettyOr is <+> foldMap pretty us </> prettyArgs ias <+> "=" <$> indent 2 (prettyImplExpr e)
    pretty (Implement _ [] is us n Nothing e) = pretty n <> prettyOr is <+> foldMap pretty us </> "=" <$> indent 2 (prettyImplExpr e)
    pretty (Implement _ ps is [] n (Just ias) e) = foldMap pretty ps </> pretty n <> prettyOr is <+> prettyArgs ias <+> "=" <$> indent 2 (prettyImplExpr e)
    pretty (Implement _ ps is [] n Nothing e)    = foldMap pretty ps </> pretty n <> prettyOr is <+> "=" <$> indent 2 (prettyImplExpr e)
    pretty (Implement _ ps is us n (Just ias) e) = foldMap pretty ps </> pretty n <> prettyOr is </> foldMap pretty us <+> prettyArgs ias <+> "=" <$> indent 2 (prettyImplExpr e)
    pretty (Implement _ ps is us n Nothing e) = foldMap pretty ps </> pretty n <> prettyOr is </> foldMap pretty us <+> "=" <$> indent 2 (prettyImplExpr e)

isVal :: Declaration a -> Bool
isVal Val{}     = True
isVal Var{}     = True
isVal PrVal{}   = True
isVal PrVar{}   = True
isVal AndDecl{} = True
isVal _         = False

isOverload :: Declaration a -> Bool
isOverload OverloadOp{}    = True
isOverload OverloadIdent{} = True
isOverload _               = False

-- isTypeDef :: Declaration a -> Bool
-- isTypeDef ViewTypeDef{} = True
-- isTypeDef TypeDef{} = True

notDefine :: String -> Bool
notDefine = not . ("#define" `isPrefixOf`)

glue :: Declaration a -> Declaration a -> Bool
glue x y
    | isVal x && isVal y = True
    | isOverload x && isOverload y = True
glue Stadef{} Stadef{}             = True
glue Load{} Load{}                 = True
glue Define{} Define{}             = True
glue Include{} Include{}           = True
glue FixityDecl{} FixityDecl{}     = True
glue ViewTypeDef{} ViewTypeDef{}   = True
glue AbsViewType{} AbsViewType{}   = True
glue AbsType{} AbsType{}           = True
glue AbsType{} AbsViewType{}       = True
glue AbsViewType{} AbsType{}       = True
glue AbsImpl{} AbsImpl{}           = True
glue TypeDef{} TypeDef{}           = True
glue Comment{} _                   = True
glue (Func _ Fnx{}) (Func _ And{}) = True
glue Assume{} Assume{}             = True
glue (Define s) _ | notDefine s    = True
glue _ (Define s) | notDefine s    = True
glue _ _                           = False

concatSame :: Eq a => [Declaration a] -> Doc
concatSame []  = mempty
concatSame [x] = pretty x
concatSame (x:x':xs)
    | glue x x' = pretty x <$> concatSame (x':xs)
    | otherwise = pretty x <> line <$> concatSame (x':xs)

($$) :: Doc -> Doc -> Doc
x $$ y = align (x <$> y)

lineAlt :: Doc -> Doc -> Doc
lineAlt = group .* flatAlt

showFast :: Doc -> String
showFast d = displayS (renderCompact d) mempty

prettyRecord :: (Pretty a) => NonEmpty (String, a) -> Doc
prettyRecord es
    | any ((>40) . length . showFast . pretty) es = prettyRecordF True (toList es)
    | otherwise                                   = lineAlt (prettyRecordF True (toList es)) (prettyRecordS True (toList es))

prettyRecordS :: (Pretty a) => Bool -> [(String, a)] -> Doc
prettyRecordS _ []             = mempty
prettyRecordS True [(s, t)]    = "@{" <+> text s <+> "=" <+> pretty t <+> "}"
prettyRecordS _ [(s, t)]       = "@{" <+> text s <+> "=" <+> pretty t
prettyRecordS True ((s, t):xs) = prettyRecordS False xs <> "," <+> text s <+> ("=" <+> pretty t) <+> "}"
prettyRecordS x ((s, t):xs)    = prettyRecordS x xs <> "," <+> text s <+> ("=" <+> pretty t)

prettyRecordF :: (Pretty a) => Bool -> [(String, a)] -> Doc
prettyRecordF _ []             = mempty
prettyRecordF True [(s, t)]    = "@{" <+> text s <+> align ("=" <+> pretty t) <+> "}"
prettyRecordF _ [(s, t)]       = "@{" <+> text s <+> align ("=" <+> pretty t)
prettyRecordF True ((s, t):xs) = prettyRecordF False xs $$ indent 1 ("," <+> text s <+> align ("=" <+> pretty t) <$> "}")
prettyRecordF x ((s, t):xs)    = prettyRecordF x xs $$ indent 1 ("," <+> text s <+> align ("=" <+> pretty t))

prettyUsNil :: Eq a => [Universal a] -> Doc
prettyUsNil [] = space
prettyUsNil us = space <> foldMap pretty us <> space

prettyUsStarNil :: Eq a => [Universal a] -> Doc
prettyUsStarNil [] = space
prettyUsStarNil us = space <> foldMap pretty us

prettyOf :: (Pretty a) => Maybe a -> Doc
prettyOf Nothing  = mempty
prettyOf (Just x) = space <> "of" <+> pretty x

prettyDL :: Eq a => [DataPropLeaf a] -> Doc
prettyDL []                        = mempty
prettyDL [DataPropLeaf us e e']    = indent 2 ("|" <> prettyUsNil us <> pretty e <> prettyOf e')
prettyDL (DataPropLeaf us e e':xs) = prettyDL xs $$ indent 2 ("|" <> prettyUsNil us <> pretty e <> prettyOf e')

prettyDSL :: Eq a => [DataSortLeaf a] -> Doc
prettyDSL []                          = mempty
prettyDSL [DataSortLeaf us sr sr']    = indent 2 ("|" <> prettyUsNil us <> pretty sr <> prettyOf sr')
prettyDSL (DataSortLeaf us sr sr':xs) = prettyDSL xs $$ indent 2 ("|" <> prettyUsNil us <> pretty sr <> prettyOf sr')

prettyLeaf :: Eq a => [Leaf a] -> Doc
prettyLeaf []                         = mempty
prettyLeaf [Leaf [] s [] Nothing]     = indent 2 ("|" <+> text s)
prettyLeaf [Leaf [] s [] (Just e)]    = indent 2 ("|" <+> text s <+> "of" <+> pretty e)
prettyLeaf (Leaf [] s [] Nothing:xs)  = prettyLeaf xs $$ indent 2 ("|" <+> text s)
prettyLeaf (Leaf [] s [] (Just e):xs) = prettyLeaf xs $$ indent 2 ("|" <+> text s <+> "of" <+> pretty e)
prettyLeaf [Leaf [] s as Nothing]     = indent 2 ("|" <+> text s <> prettyArgs as)
prettyLeaf [Leaf [] s as (Just e)]    = indent 2 ("|" <+> text s <> prettyArgs as <+> "of" <+> pretty e)
prettyLeaf (Leaf [] s as Nothing:xs)  = prettyLeaf xs $$ indent 2 ("|" <+> text s <> prettyArgs as)
prettyLeaf (Leaf [] s as (Just e):xs) = prettyLeaf xs $$ indent 2 ("|" <+> text s <> prettyArgs as <+> "of" <+> pretty e)
prettyLeaf [Leaf us s [] Nothing]     = indent 2 ("|" <+> fancyU us <+> text s)
prettyLeaf [Leaf us s [] (Just e)]    = indent 2 ("|" <+> fancyU us <+> text s <+> "of" <+> pretty e)
prettyLeaf (Leaf us s [] Nothing:xs)  = prettyLeaf xs $$ indent 2 ("|" <+> fancyU us <+> text s)
prettyLeaf (Leaf us s [] (Just e):xs) = prettyLeaf xs $$ indent 2 ("|" <+> fancyU us <+> text s <+> "of" <+> pretty e)
prettyLeaf [Leaf us s as Nothing]     = indent 2 ("|" <+> fancyU us <+> text s <> prettyArgs as)
prettyLeaf [Leaf us s as (Just e)]    = indent 2 ("|" <+> fancyU us <+> text s <> prettyArgs as <+> "of" <+> pretty e)
prettyLeaf (Leaf us s as Nothing:xs)  = prettyLeaf xs $$ indent 2 ("|" <+> fancyU us <+> text s <> prettyArgs as)
prettyLeaf (Leaf us s as (Just e):xs) = prettyLeaf xs $$ indent 2 ("|" <+> fancyU us <+> text s <> prettyArgs as <+> "of" <+> pretty e)

prettyHelper :: Doc -> [Doc] -> [Doc]
prettyHelper _ [x]    = [x]
prettyHelper c (x:xs) = flatAlt (" " <> x) x : fmap (c <>) xs
prettyHelper _ x      = x

prettyBody :: Doc -> Doc -> [Doc] -> Doc
prettyBody c1 c2 [d] = c1 <> d <> c2
prettyBody c1 c2 ds  = (c1 <>) . align . indent (-1) . cat . (<> pure c2) $ ds

prettyArgsG' :: Foldable t => Doc -> Doc -> Doc -> t Doc -> Doc
prettyArgsG' c3 c1 c2 = prettyBody c1 c2 . prettyHelper c3 . reverse . toList

prettyArgsList :: Foldable t => Doc -> Doc -> Doc -> t Doc -> Doc
prettyArgsList c3 c1 c2 = prettyBody c1 c2 . va . prettyHelper c3 . toList
    where va = (& _tail.traverse %~ group)

prettyArgsG :: Foldable t => Doc -> Doc -> t Doc -> Doc
prettyArgsG = prettyArgsG' ", "

prettyArgsU :: (Pretty a, Foldable f, Functor f) => Doc -> Doc -> f a -> Doc
prettyArgsU = prettyArgs' ","

prettyArgs' :: (Pretty a, Functor f, Foldable f) => Doc -> Doc -> Doc -> f a -> Doc
prettyArgs' = fmap pretty .@@@ prettyArgsG'

prettyArgs :: (Pretty a, Foldable f, Functor f) => f a -> Doc
prettyArgs = prettyArgs' ", " "(" ")"

prettyArgsNil :: Eq a => Maybe [Arg a] -> Doc
prettyArgsNil Nothing   = mempty
prettyArgsNil (Just as) = prettyArgs' ", " "(" ")" as

fancyU :: Pretty a => [a] -> Doc
fancyU = foldMap pretty

(<#>) :: Doc -> Doc -> Doc
(<#>) a b = lineAlt (a <$> indent 2 b) (a <+> b)

prettySigG :: (Pretty a) => Doc -> Doc -> Maybe String -> Maybe a -> Doc
prettySigG d d' (Just si) (Just rt) = d `op` ":" <> text si <#> pretty rt <> d'
    where op a b = lineAlt (a <$> indent 2 b) (a <> b)
prettySigG _ _ _ _                  = mempty

prettySigNull :: (Pretty a) => Maybe String -> Maybe a -> Doc
prettySigNull = prettySigG space mempty

prettySig :: (Pretty a) => Maybe String -> Maybe a -> Doc
prettySig = prettySigG space space

prettyTermetric :: Pretty a => a -> Doc
prettyTermetric t = softline <> ".<" <> pretty t <> ">." <> softline

prettyETermetric :: Pretty a => Maybe a -> Doc
prettyETermetric Nothing  = softline <> ".<>." <> softline
prettyETermetric (Just t) = softline <> ".<" <> pretty t <> ">." <> softline

prettyMTermetric :: Pretty a => Maybe (Maybe a) -> Doc
prettyMTermetric = maybe mempty prettyETermetric

-- FIXME figure out a nicer algorithm for when/how to split lines.
instance (Eq a, Pretty (ek a)) => Pretty (PreFunction ek a) where
    pretty (PreF i si [] [] as rt Nothing (Just e)) = pretty i <> prettyArgsNil as <> prettySig si rt <> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] us as rt t (Just e)) = pretty i </> fancyU us <> prettyMTermetric t <> prettyArgsNil as <> prettySig si rt <> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus [] as rt Nothing (Just e)) = fancyU pus </> pretty i <> prettyArgsNil as <> prettySig si rt <> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus us as rt t (Just e)) = fancyU pus </> pretty i </> fancyU us <> prettyMTermetric t <> prettyArgsNil as <> prettySig si rt <> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] [] as rt Nothing Nothing) = pretty i <> prettyArgsNil as <> prettySigNull si rt
    pretty (PreF i si [] us Nothing rt Nothing Nothing) = pretty i </> fancyU us <> prettySigNull si rt
    pretty (PreF i si [] us as rt Nothing Nothing) = pretty i </> fancyU us </> prettyArgsNil as <> prettySigNull si rt
    pretty (PreF i si pus [] as rt t Nothing) = fancyU pus </> pretty i <> prettyMTermetric t </> prettyArgsNil as <> prettySigNull si rt
    pretty (PreF i si pus us as rt t Nothing) = fancyU pus </> pretty i <> prettyMTermetric t </> fancyU us </> prettyArgsNil as <> prettySigNull si rt

prettyFix :: (Pretty a) => Either a String -> Doc
prettyFix (Left i)  = pretty i
prettyFix (Right s) = parens (text s)

instance Eq a => Pretty (Fixity a) where
    pretty (Infix _ i)    = "infix" <+> prettyFix i
    pretty (RightFix _ i) = "infixr" <+> prettyFix i
    pretty (LeftFix _ i)  = "infixl" <+> prettyFix i
    pretty (Pre _ i)      = "prefix" <+> prettyFix i
    pretty (Post _ i)     = "postfix" <+> prettyFix i

prettyMaybeType :: (Pretty a) => Maybe a -> Doc
prettyMaybeType (Just a) = " =" <+> pretty a
prettyMaybeType _        = mempty

valSig :: (Pretty a) => Maybe a -> Doc
valSig = prettySigG mempty mempty (Just mempty)

prettySortArgs :: (Pretty a) => Maybe [a] -> Doc
prettySortArgs = maybe mempty (prettyArgs' ", " "(" ")")

maybeT :: Pretty a => Maybe a -> Doc
maybeT (Just x) = ":" <+> pretty x
maybeT Nothing  = mempty

instance Eq a => Pretty (Declaration a) where
    pretty (Exception s t)                  = "exception" <+> text s <+> "of" <+> pretty t
    pretty (AbsType _ s as t)               = "abstype" <+> text s <> prettySortArgs as <> prettyMaybeType t
    pretty (AbsViewType _ s as Nothing)     = "absvtype" <+> text s <> prettySortArgs as
    pretty (AbsViewType _ s as (Just t))    = "absvtype" <+> text s <> prettySortArgs as <+> "=" <+> pretty t
    pretty (SumViewType s as ls)            = "datavtype" <+> text s <> prettySortArgs as <+> "=" <$> prettyLeaf (toList ls)
    pretty (AndD d (SumViewType s as ls))   = pretty d <$> "and" <+> text s <> prettySortArgs as <+> "=" <$> prettyLeaf (toList ls)
    pretty (AndD d (SumType s as ls))       = pretty d <$> "and" <+> text s <> prettySortArgs as <+> "=" <$> prettyLeaf (toList ls)
    pretty (AndD d (DataView _ s as ls))    = pretty d <$> "and" <+> text s <> prettySortArgs as <+> "=" <$> prettyLeaf (toList ls)
    pretty (DataView _ s as ls)             = "dataview" <+> text s <> prettySortArgs as <+> "=" <$> prettyLeaf (toList ls)
    pretty (SumType s as ls)                = "datatype" <+> text s <> prettySortArgs as <+> "=" <$> prettyLeaf (toList ls)
    pretty (DataSort _ s ls)                = "datasort" <+> text s <+> "=" <$> prettyDSL (toList ls)
    pretty (Impl as i)                      = "implement" <+> prettyArgsNil as <> pretty i
    pretty (ProofImpl as i)                 = "primplmnt" <+> prettyArgsNil as <> pretty i
    pretty (PrVal us p (Just e) Nothing)    = "prval" <> prettyUsNil us <> pretty p <+> "=" <+> pretty e
    pretty (PrVal us p Nothing (Just t))    = "prval" <> prettyUsNil us <> pretty p <+> ":" <+> pretty t
    pretty (PrVal us p (Just e) (Just t))   = "prval" <> prettyUsNil us <> pretty p <+> ":" <+> pretty t <+> "=" <+> pretty e
    pretty PrVal{}                          = undefined
    pretty (PrVar p (Just e) Nothing)       = "prvar" <+> pretty p <+> "=" <+> pretty e
    pretty (PrVar p Nothing (Just t))       = "prvar" <+> pretty p <+> ":" <+> pretty t
    pretty (PrVar p (Just e) (Just t))      = "prvar" <+> pretty p <+> ":" <+> pretty t <+> "=" <+> pretty e
    pretty PrVar{}                          = undefined
    pretty (AndDecl t p e)                  = "and" <+> pretty p <> valSig t <+> "=" <+> pretty e
    pretty (Val a t p (Just e))             = "val" <> pretty a <+> pretty p <> valSig t <+> "=" <+> pretty e
    pretty (Val a t p Nothing)              = "val" <> pretty a <+> pretty p <> valSig t
    pretty (Var t p Nothing (Just e))       = "var" <+> pretty p <> valSig t <+> "with" <+> pretty e
    pretty (Var t p (Just e) Nothing)       = "var" <+> pretty p <> valSig t <+> "=" <+> pretty e
    pretty (Var t p Nothing Nothing)        = "var" <+> pretty p <> valSig t
    pretty (Var _ _ _ Just{})               = undefined -- TODO figure out what this is supposed to be
    pretty (Include s)                      = "#include" <+> pretty s
    pretty (Load sta b Nothing s)           = bool "" "#" b <> bool "dyn" "sta" sta <> "load" <+> pretty s
    pretty (Load sta b (Just q) s)          = bool "" "#" b <> bool "dyn" "sta" sta <> "load" <+> pretty q <+> "=" <+> pretty s
    pretty (CBlock s)                       = string s
    pretty (Comment s)                      = string s
    pretty (OverloadOp _ o n (Just n'))     = "overload" <+> pretty o <+> "with" <+> pretty n <+> "of" <+> pretty n'
    pretty (OverloadOp _ o n Nothing)       = "overload" <+> pretty o <+> "with" <+> pretty n
    pretty (OverloadIdent _ i n Nothing)    = "overload" <+> text i <+> "with" <+> pretty n
    pretty (OverloadIdent _ i n (Just n'))  = "overload" <+> text i <+> "with" <+> pretty n <+> "of" <+> pretty n'
    -- We use 'text' here, which means indentation might get fucked up for
    -- C preprocessor macros, but you absolutely deserve it if you indent your
    -- macros.
    pretty (Define s) | "#if" `isPrefixOf` s = text ("\n" <> s)
                      | otherwise            = text s
    pretty (Func _ (Fn pref))               = "fn" </> pretty pref
    pretty (Func _ (Fun pref))              = "fun" </> pretty pref
    pretty (Func _ (CastFn pref))           = "castfn" </> pretty pref
    pretty (Func _ (Fnx pref))              = "fnx" </> pretty pref
    pretty (Func _ (And pref))              = "and" </> pretty pref
    pretty (Func _ (Praxi pref))            = "praxi" </> pretty pref
    pretty (Func _ (PrFun pref))            = "prfun" </> pretty pref
    pretty (Func _ (PrFn pref))             = "prfn" </> pretty pref
    pretty (Extern _ d)                     = "extern" <$> pretty d
    pretty (DataProp _ s as ls)             = "dataprop" <+> text s <> prettySortArgs as <+> "=" <$> prettyDL ls
    pretty (ViewTypeDef _ s as t)           = "vtypedef" <+> text s <> prettySortArgs as <+> "=" <#> pretty t
    pretty (TypeDef _ s as t ms)            = "typedef" <+> text s <> prettySortArgs as <+> "=" <+> pretty t <> maybeT ms
    pretty (AbsProp _ n as)                 = "absprop" <+> text n <+> prettyArgs as
    pretty (Assume n as e)                  = "assume" </> pretty n <> prettySortArgs as <+> "=" </> pretty e
    pretty (SymIntr _ ns)                   = "symintr" <+> hsep (fmap pretty ns)
    pretty (Stacst _ n t Nothing)           = "stacst" </> pretty n <+> ":" </> pretty t
    pretty (Stacst _ n t (Just e))          = "stacst" </> pretty n <+> ":" </> pretty t <+> "=" </> pretty e
    pretty (PropDef _ s as@Just{} t)        = "propdef" </> text s <+> prettyArgsNil as <+> "=" </> pretty t
    pretty (PropDef _ s Nothing t)          = "propdef" </> text s <+> "=" </> pretty t
    pretty (Local _ (ATS ds) (ATS []))      = "local" <$> indent 2 (pretty (ATS ds)) <$> "in end"
    pretty (Local _ d d')                   = "local" <$> indent 2 (pretty d) <$> "in" <$> indent 2 (pretty d') <$> "end"
    pretty (FixityDecl f ss)                = pretty f <+> hsep (fmap text ss)
    pretty (StaVal us i t)                  = "val" </> mconcat (fmap pretty us) <+> text i <+> ":" <+> pretty t
    pretty (Stadef i as (Right (Nothing, t))) = "stadef" <+> text i <+> prettySortArgs as <+> "=" <+> pretty t
    pretty (Stadef i as (Right (Just ty, t))) = "stadef" <+> text i <+> prettySortArgs as <+> ":" <+> pretty ty <+> "=" <+> pretty t
    pretty (Stadef i as (Left (se, mt)))    = "stadef" <+> text i <+> prettySortArgs as <+> "=" <+> pretty se <> maybeT mt
    pretty (AndD d (Stadef i as (Right (Nothing, t)))) = pretty d <+> "and" <+> text i <+> prettySortArgs as <+> "=" <+> pretty t
    pretty (AndD d (Stadef i as (Right (Just ty, t)))) = pretty d <+> "and" <+> text i <+> prettySortArgs as <+> ":" <+> pretty ty <+> "=" <+> pretty t
    pretty (AndD d (Stadef i as (Left (se, mt)))) = pretty d <+> "and" <+> text i <+> prettySortArgs as <+> "=" <+> pretty se <> maybeT mt
    pretty (AbsView _ i as t)               = "absview" <+> text i <> prettySortArgs as <> prettyMaybeType t
    pretty (AbsVT0p _ i as t)               = "absvt@ype" <+> text i <> prettySortArgs as <> prettyMaybeType t
    pretty (AbsT0p _ i Nothing t)           = "abst@ype" <+> text i <+> "=" <+> pretty t
    pretty (AbsT0p _ i as t)                = "abst@ype" <+> text i <> prettySortArgs as <> "=" <+> pretty t
    pretty (ViewDef _ s as t)               = "viewdef" <+> text s <> prettySortArgs as <+> "=" <#> pretty t
    pretty (TKind _ n s)                    = pretty n <+> "=" <+> text s
    pretty (SortDef _ s t)                  = "sortdef" <+> text s <+> "=" <+> either pretty pretty t
    pretty (AndD d (SortDef _ i t))         = pretty d <+> "and" <+> text i <+> "=" <+> either pretty pretty t
    pretty (MacDecl _ n (Just is) e)        = "macdef" <+> text n <> "(" <> mconcat (punctuate ", " (fmap text is)) <> ") =" <+> pretty e
    pretty (MacDecl _ n Nothing e)          = "macdef" <+> text n <+> "=" <+> pretty e
    pretty (ExtVar _ s e)                   = "extvar" <+> text s <+> "=" <+> pretty e
    pretty (AbsImpl _ n as e)               = "absimpl" </> pretty n <> prettySortArgs as <+> "=" </> pretty e
    pretty AndD{}                           = undefined -- probably not valid syntax if we get to this point
