{-# LANGUAGE PatternSynonyms #-}

module Language.ATS.Generate
    ( -- * Functions
      generateATS
    , genATSTypes
    -- * Types
    , ErrM
    , GenerateError (..)
    ) where

import           Control.Arrow
import           Data.Bool                    (bool)
import           Data.Char                    (toUpper)
import           Data.Either                  (lefts, rights)
import           Data.Maybe
import           Language.ATS                 as ATS
import           Language.ATS.Generate.Error
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Syntax as HS
import           Language.Preprocessor.Cpphs  (defaultCpphsOptions, runCpphs)
import           Lens.Micro                   (over, _head)
import           Text.Casing                  (quietSnake)

convertConventions :: String -> String
convertConventions = filterKeys . quietSnake

pattern QNamed :: l -> l -> String -> QName l
pattern QNamed x y s = UnQual x (Ident y s)

pattern QStorable :: l -> l -> QName l
pattern QStorable x y = UnQual x (Ident y "StorableWrapper")

pattern EmptyQualCon :: l -> ConDecl l -> QualConDecl l
pattern EmptyQualCon x cd = QualConDecl x Nothing Nothing cd

filterKeys :: String -> String
filterKeys "var" = "var_"
filterKeys s     = s

qnameToString :: QName a -> ErrM String
qnameToString (QNamed _ _ "Maybe") = Right "Option_vt"
qnameToString (QNamed _ _ s)       = Right $ convertConventions s
qnameToString _                    = unsupported "qnameToString"

-- should we allow user-defined string map?
stringTypeConv :: String -> ErrM String
stringTypeConv "Integer" = Right "Intinf"
stringTypeConv "String"  = Right "Strptr1"
stringTypeConv "CString" = Right "Strptr1"
stringTypeConv "Word"    = Right "uint"
stringTypeConv "CUInt"   = Right "uint"
stringTypeConv "Int"     = Right "int"
stringTypeConv "CInt"    = Right "int"
stringTypeConv "Float"   = Right "float"
stringTypeConv "CFloat"  = Right "float"
stringTypeConv "Double"  = Right "double"
stringTypeConv "Bool"    = Right "bool"
stringTypeConv "CBool"   = Right "bool"
stringTypeConv _         = unsupported "stringTypeConv"

toStringATS' :: QName a -> ErrM (ATS.Type b)
toStringATS' (QNamed _ _ s) = Named . Unqualified <$> stringTypeConv s
toStringATS' _              = unsupported "toStringATS'"

tyVarToSort :: TyVarBind a -> ErrM (Universal b)
tyVarToSort (UnkindedVar _ (Ident _ s)) = Right $ Universal [s] (Just (Vt0p None)) mempty
tyVarToSort _                           = unsupported "tyVarToSort"

universalHelper :: [TyVarBind a] -> ErrM (ATS.Type b -> ATS.Type b)
universalHelper (t:ts) = fmap <$> (ForA <$> tyVarToSort t) <*> universalHelper ts
universalHelper []     = pure id

typeToType :: HS.Type a -> ErrM (ATS.Type b)
typeToType (TyForall _ (Just us) Nothing t)   = universalHelper us <*> typeToType t
typeToType (TyCon _ qn)                       = toStringATS' qn
typeToType (TyVar _ n)                        = Right $ Named $ Unqualified (toStringATS n)
typeToType (TyApp _ (TyCon _ QStorable{}) t') = typeToType t'
typeToType (TyApp _ (TyCon _ qn) t')          = Dependent <$> (Unqualified <$> qnameToString qn) <*> (pure <$> typeToType t')
typeToType (TyApp _ t@TyApp{} t')             = over typeCallArgs <$> fmap (:) (typeToType t') <*> typeToType t
typeToType (TyParen _ t)                      = typeToType t
typeToType (TyBang _ _ _ t)                   = typeToType t
typeToType (TyFun _ t t')                     = FunctionType "-<lincloptr1>" <$> typeToType t <*> typeToType t'
typeToType (TyTuple _ _ ts)                   = ATS.Tuple undefined <$> mapM typeToType ts
typeToType _                                  = Left $ Unsupported "typeToType"

fieldDeclToType :: FieldDecl a -> ErrM (String, ATS.Type b)
fieldDeclToType (FieldDecl _ [n] t) = (,) (toStringATS n) <$> typeToType t
fieldDeclToType _                   = Left $ Unsupported "fieldDeclToType"

conDeclToType :: ConDecl a -> ErrM (String, Maybe (ATS.Type b))
conDeclToType (ConDecl _ n [])  = Right (toStringATS n, Nothing)
conDeclToType (ConDecl _ n [t]) = (,) (toStringATS n) . Just <$> typeToType t
conDeclToType (ConDecl _ n ts)  = (,) (toStringATS n) . Just . ATS.Tuple undefined <$> mapM typeToType ts
conDeclToType (RecDecl _ n fs)  = (,) (toStringATS n) . Just . AnonymousRecord undefined <$> mapM fieldDeclToType (reverse fs)
conDeclToType _                 = unsupported "conDeclToType"

toStringATS :: HS.Name a -> String
toStringATS (Ident _ s) = s
toStringATS _           = undefined

tyvarToArg :: Bool -> TyVarBind a -> ErrM (SortArg b)
tyvarToArg False (UnkindedVar _ n) = Right $ SortArg (toStringATS n) (Vt0p None)
tyvarToArg True (UnkindedVar _ n)  = Right $ SortArg (toStringATS n) (Vt0p Plus)
tyvarToArg _ _                     = unsupported "tyvarToArg"

consM :: (Monad m) => m a -> m [a] -> m [a]
consM x xs = (:) <$> x <*> xs

asATSName :: DeclHead a -> ErrM (String, [SortArg b])
asATSName (DHead _ n)    = Right (convertConventions $ toStringATS n, [])
asATSName (DHParen _ d)  = (,) . fst <$> asATSName d <*> pure []
asATSName (DHApp _ d tb) = (,) . fst <$> asATSName d <*> consM (tyvarToArg False tb) (snd <$> asATSName d)
asATSName _              = unsupported "asATSName"

qualConDeclToType :: QualConDecl a -> ErrM (ATS.Type b)
qualConDeclToType (EmptyQualCon _ cd) = fromJust . snd <$> conDeclToType cd
qualConDeclToType _                   = unsupported "qualConDeclToType"

qualConDeclToLeaf :: QualConDecl a -> ErrM (Leaf b)
qualConDeclToLeaf (EmptyQualCon _ cd) = Leaf [] <$> (over _head toUpper . convertConventions . fst <$> conDeclToType cd) <*> pure [] <*> (snd <$> conDeclToType cd)
qualConDeclToLeaf _                   = unsupported "qualConDeclToLeaf"

pruneATSNils :: [SortArg a] -> Maybe [SortArg a]
pruneATSNils [] = Nothing
pruneATSNils x  = Just x

-- TODO if it derives functor, use +
asATSType :: Decl a -> ErrM (Declaration b)
asATSType (TypeDecl _ dh t) = ViewTypeDef undefined <$> (fst <$> asATSName dh) <*> (pruneATSNils . snd <$> asATSName dh) <*> typeToType t
asATSType (DataDecl _ NewType{} _ dh [qcd] _)  = ViewTypeDef undefined <$> (fst <$> asATSName dh) <*> (pruneATSNils . snd <$> asATSName dh) <*> qualConDeclToType qcd
asATSType (DataDecl _ DataType{} _ dh [qcd] _) = ViewTypeDef undefined <$> (fst <$> asATSName dh) <*> (pruneATSNils . snd <$> asATSName dh) <*> qualConDeclToType qcd
asATSType (DataDecl _ DataType{} _ dh qcds _)  = SumViewType <$> (fst <$> asATSName dh) <*> (pruneATSNils . snd <$> asATSName dh) <*> mapM qualConDeclToLeaf (reverse qcds)
asATSType _                                    = unsupported "asATSType"

-- TODO GDataDecl
isDataDecl :: Decl a -> Bool
isDataDecl TypeDecl{} = True
isDataDecl DataDecl{} = True
isDataDecl _          = False

filterModule :: Module a -> [Decl a]
filterModule (Module _ _ _ _ ds) = filter isDataDecl ds
filterModule _                   = []

modulePrint :: Module a -> (String, [GenerateError])
modulePrint = g . fmap asATSType . filterModule
    where g = (h . ATS . reverse . rights) &&& lefts
          h :: ATS AlexPosn -> String
          h = printATS

extends :: ParseMode
extends =
    defaultParseMode { extensions = EnableExtension <$> es, fixities = Just baseFixities }

    where es = [ StandaloneDeriving
               , CPP
               , RecordWildCards
               , BangPatterns
               , ExplicitForAll
               , FlexibleContexts
               ]

-- | Given a string containing Haskell, return a string containing ATS and
-- a list of warnings.
generateATS :: FilePath -- ^ Name of source file
            -> String -- ^ Source file contents
            -> ErrM (String, [GenerateError])
generateATS file hsSrc = modulePrint <$> case parseModuleWithMode extends hsSrc of
    ParseOk x            -> Right x
    ParseFailed loc' msg -> syntaxError (loc' { srcFilename = file }) msg

process :: FilePath -> String -> IO String
process p = fmap (unlines . drop 1 . lines) . runCpphs defaultCpphsOptions p

genATSTypes :: FilePath -- ^ Haskell source file
            -> FilePath -- ^ @.sats@ file to be generated
            -> Bool -- ^ Whether to use pre-process the Haskell source (use this if you use @{#- LANGUAGE CPP #-}@ anywhere)
            -> IO ()
genATSTypes p p' withCPP = do
    let proc = bool pure (process p) withCPP
    contents <- proc =<< readFile p
    let warnDo (x, es) = mapM_ displayErr es >> writeFile p' x
    either displayErr warnDo (generateATS p contents)
