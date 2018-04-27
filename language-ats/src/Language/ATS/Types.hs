{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | This is a module containing types to model the ATS syntax tree. As it is
-- collapsed by the pretty printer, you may see that in some places it is
-- focused on the lexical side of things.
module Language.ATS.Types
    ( ATS (..)
    , Declaration (..)
    , Type (..)
    , Name (..)
    , Pattern (..)
    , PatternF (..)
    , Arg (..)
    , Universal (..)
    , Function (..)
    , Expression (..)
    , ExpressionF (..)
    , Implementation (..)
    , BinOp (..)
    , BinOp (Con)
    , UnOp (..)
    , TypeF (..)
    , Existential (..)
    , LambdaType (..)
    , Addendum (..)
    , DataPropLeaf (..)
    , PreFunction (..)
    , Paired (..)
    , Leaf (..)
    , StaticExpression (..)
    , StaticExpressionF (..)
    , Fixity (..)
    , StackFunction (..)
    , Sort (..)
    , SortArg (..)
    , SortArgs
    , DataSortLeaf (..)
    , FixityState
    , Fix
    -- * Rewrites
    , rewriteDecl
    -- * Helper functions
    , defaultFixityState
    -- * Lenses
    , preF
    , expression
    , fun
    , leaves
    , constructorUniversals
    , typeCall
    , typeCallArgs
    , comment
    ) where

import           Control.Composition
import           Control.DeepSeq          (NFData)
import           Control.Lens
import           Control.Monad
import           Data.Function            (on)
import           Data.Functor.Foldable    (ListF (Cons), ana, cata, embed, project)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.Map                 as M
import           Data.Maybe               (isJust)
import           Data.Semigroup           (Semigroup)
import           GHC.Generics             (Generic)
import           Language.ATS.Lexer       (Addendum (..))

type Fix = Either Int String

data Fixity a = RightFix { pos :: a, ifix :: Fix }
              | LeftFix { pos :: a, ifix :: Fix }
              | Pre { pos :: a, ifix :: Fix }
              | Post { pos :: a, ifix :: Fix }
              | Infix { pos :: a, ifix :: Fix }
              deriving (Show, Eq, Generic, NFData)

-- | An ATS file, containing a list of declarations
newtype ATS a = ATS { unATS :: [Declaration a] }
    deriving (Show, Eq, Generic)
    deriving newtype (NFData, Semigroup, Monoid)

data Leaf a = Leaf { _constructorUniversals :: [Universal a], name :: String, constructorArgs :: [StaticExpression a], maybeType :: Maybe (Type a) }
    deriving (Show, Eq, Generic, NFData)

type SortArgs a = Maybe [SortArg a]

-- | Declarations for functions, values, actions, etc.
data Declaration a = Func { pos :: a, _fun :: Function a }
                   | Impl { implArgs :: [Arg a], _impl :: Implementation a } -- TODO do something better for implicit universals
                   | ProofImpl { implArgs :: [Arg a], _impl :: Implementation a }
                   | Val { add :: Addendum, valT :: Maybe (Type a), valPat :: Pattern a, _valExpression :: Expression a }
                   | StaVal [Universal a] String (Type a)
                   | PrVal { prvalPat :: Pattern a, _prValExpr :: Maybe (Expression a), prValType :: Maybe (Type a) }
                   | Var { varT :: Maybe (Type a), varPat :: Pattern a, _varExpr1 :: Maybe (Expression a), _varExpr2 :: Maybe (Expression a) }
                   | AndDecl { andT :: Maybe (Type a), andPat :: Pattern a, _andExpr :: Expression a }
                   | Include String
                   | Load { static :: Bool, withOctothorpe :: Bool, qualName :: Maybe String, fileName :: String }
                   | Stadef String (SortArgs a) (Either (StaticExpression a, Maybe (Sort a)) (Type a))
                   | CBlock String
                   | TypeDef a String (SortArgs a) (Type a) (Maybe (Sort a))
                   | ViewTypeDef a String (SortArgs a) (Type a)
                   | SumType { typeName :: String, typeArgs :: SortArgs a, _leaves :: [Leaf a] }
                   | SumViewType { typeName :: String, typeArgs :: SortArgs a, _leaves :: [Leaf a] }
                   | AbsType a String (SortArgs a) (Maybe (Type a))
                   | AbsViewType a String (SortArgs a) (Maybe (Type a))
                   | AbsView a String (SortArgs a) (Maybe (Type a))
                   | AbsVT0p a String (SortArgs a) (Maybe (Type a))
                   | AbsT0p a String (SortArgs a) (Maybe (Type a))
                   | ViewDef a String (SortArgs a) (Type a)
                   | OverloadOp a (BinOp a) (Name a) (Maybe Int)
                   | OverloadIdent a String (Name a) (Maybe Int)
                   | Comment { _comment :: String }
                   | DataProp { pos :: a, propName :: String, propArgs :: SortArgs a, _propLeaves :: [DataPropLeaf a] }
                   | DataView a String (SortArgs a) [Leaf a]
                   | Extern a (Declaration a)
                   | Define String
                   | SortDef a String (Either (Sort a) (Universal a))
                   | AndD (Declaration a) (Declaration a)
                   | Local a (ATS a) (ATS a)
                   | AbsProp a String [Arg a]
                   | Assume (Name a) [Arg a] (Type a)
                   | TKind a (Name a) String
                   | SymIntr a [Name a]
                   | Stacst a (Name a) (Type a) (Maybe (Expression a))
                   | PropDef a String [Arg a] (Type a)
                   | FixityDecl (Fixity a) [String]
                   | MacDecl a String [String] (Expression a)
                   | DataSort a String [DataSortLeaf a]
                   | Exception String (Type a)
                   | ExtVar a String (Expression a)
                   deriving (Show, Eq, Generic, NFData)

data DataSortLeaf a = DataSortLeaf [Universal a] (Sort a) (Maybe (Sort a))
                    deriving (Show, Eq, Generic, NFData)

data DataPropLeaf a = DataPropLeaf { propU :: [Universal a], _propExpr1 :: Expression a, _propExpr2 :: Maybe (Expression a) }
                    deriving (Show, Eq, Generic, NFData)

-- | A type for parsed ATS types
data Type a = Tuple a [Type a]
            | BoxTuple a [Type a]
            | Named (Name a)
            | Ex (Existential a) (Maybe (Type a))
            | ForA (Universal a) (Type a)
            | Dependent { _typeCall :: Name a, _typeCallArgs :: [Type a] }
            | Unconsumed (Type a) -- @!a@
            | AsProof (Type a) (Maybe (Type a)) -- @a >> b@
            | FromVT (Type a) -- | @a?!@
            | MaybeVal (Type a) -- | @a?@
            | AtExpr a (Type a) (StaticExpression a)
            | AtType a (Type a)
            | ProofType a [Type a] (Type a) -- Aka (prf | val)
            | ConcreteType (StaticExpression a)
            | RefType (Type a)
            | ViewType a (Type a)
            | FunctionType String (Type a) (Type a)
            | NoneType a
            | ImplicitType a
            | ViewLiteral Addendum
            | AnonymousRecord a [(String, Type a)]
            | ParenType a (Type a)
            | WhereType a (Type a) String (SortArgs a) (Type a)
            deriving (Show, Eq, Generic, NFData)

-- | A type for @=>@, @=\<cloref1>@, etc.
data LambdaType a = Plain a
                  | Spear a -- | @=>>@
                  | ProofArrow a -- | @=/=>@
                  | Full a String
                  -- TODO figure out wtf ProofArrow does
                  deriving (Show, Eq, Generic, NFData)

data Name a = Unqualified String
            | Qualified a String String -- ^ A name can be qualified e.g. @$UN.unsafefn@
            | SpecialName a String -- ^ A name for builtin functions such as @$showtype@.
            | Functorial String String
            | FieldName a String String
            deriving (Show, Eq, Generic, NFData)

-- | A data type for patterns.
data Pattern a = Wildcard a
             | PName (Name a) [Pattern a]
             | PSum String (Pattern a)
             | PLiteral (Expression a)
             | Guarded a (Expression a) (Pattern a)
             | Free (Pattern a)
             | Proof a [Pattern a] [Pattern a]
             | TuplePattern [Pattern a]
             | BoxTuplePattern a [Pattern a]
             | AtPattern a (Pattern a)
             | UniversalPattern a String [Universal a] (Pattern a)
             | ExistentialPattern (Existential a) (Pattern a)
             deriving (Show, Eq, Generic, NFData)

data Paired a b = Both a b
                | First a
                | Second b
                deriving (Show, Eq, Generic, NFData)

data SortArg a = SortArg String (Sort a)
               | Anonymous (Sort a)
    deriving (Show, Eq, Generic, NFData)

-- | An argument to a function.
data Arg a = Arg (Paired String (Type a))
           | PrfArg [Arg a] (Arg a)
           | NoArgs
           deriving (Show, Eq, Generic, NFData)

-- | A datatype for sorts.
data Sort a = NamedSort { _sortName :: String }
            | T0p Addendum -- ^ @t\@ype@
            | Vt0p Addendum -- ^ @vt\@ype@
            | Addr
            | VType a Addendum -- ^ @viewtype@ or @vtype@
            | View a Addendum -- ^ @view@
            | TupleSort a (Sort a) (Sort a)
            deriving (Show, Eq, Generic, NFData)

-- FIXME a type for sorts?
-- | Wrapper for universal quantifiers (refinement types)
data Universal a = Universal { bound :: [String], typeU :: Maybe (Sort a), prop :: [StaticExpression a] }
    deriving (Show, Eq, Generic, NFData)

-- | Wrapper for existential quantifiers/types
data Existential a = Existential { boundE :: [String], isOpen :: Bool, typeE :: Maybe (Sort a), propE :: Maybe (StaticExpression a) }
    deriving (Show, Eq, Generic, NFData)

-- | Unary operators
data UnOp a = Negate -- | @~@
            | Deref -- | @!@
            | SpecialOp a String
    deriving (Show, Eq, Generic, NFData)

-- | Binary operators on expressions
data BinOp a = Add
             | Mult
             | Div
             | Sub
             | GreaterThan
             | GreaterThanEq
             | LessThan
             | LessThanEq
             | Equal
             | LogicalAnd
             | LogicalOr
             | StaticEq
             | Mod
             | NotEq
             | Mutate -- ^ @:=@
             | At
             | SpearOp -- ^ @->@
             | SpecialInfix a String
             deriving (Show, Eq, Generic, NFData)

pattern Con :: a -> BinOp a
pattern Con l = SpecialInfix l "::"

data StaticExpression a = StaticVal (Name a)
                        | StaticBinary (BinOp a) (StaticExpression a) (StaticExpression a)
                        | StaticInt Int
                        | SPrecede (StaticExpression a) (StaticExpression a)
                        | StaticVoid a
                        | Sif { scond :: StaticExpression a, wwhenTrue :: StaticExpression a, selseExpr :: StaticExpression a } -- Static if (for proofs)
                        | SCall (Name a) [StaticExpression a]
                        | SUnary (UnOp a) (StaticExpression a)
                        | SLet a [Declaration a] (Maybe (StaticExpression a))
                        | SCase Addendum (StaticExpression a) [(Pattern a, LambdaType a, StaticExpression a)]
                        deriving (Show, Eq, Generic, NFData)

-- | A (possibly effectful) expression.
data Expression a = Let a (ATS a) (Maybe (Expression a))
                  | VoidLiteral a -- ^ The '()' literal representing inaction.
                  | Call { callName       :: Name a
                         , callImplicits  :: [[Type a]] -- ^ E.g. @some_function<a>@
                         , callUniversals :: [Type a] -- ^ E.g. @some_function{a}@
                         , callProofs     :: Maybe [Expression a] -- ^ E.g. @pf@ in @call(pf | str)@.
                         , callArgs       :: [Expression a] -- ^ The actual call arguments.
                         }
                  | NamedVal (Name a)
                  | ListLiteral a String (Type a) [Expression a]
                  | If { cond     :: Expression a -- ^ Expression evaluating to a boolean value
                       , whenTrue :: Expression a -- ^ Expression to be returned when true
                       , elseExpr :: Maybe (Expression a) -- ^ Expression to be returned when false
                       }
                  | UintLit Word -- ^ E.g. @1000u@
                  | FloatLit Float
                  | IntLit Int
                  | UnderscoreLit a
                  | Lambda a (LambdaType a) (Pattern a) (Expression a)
                  | LinearLambda a (LambdaType a) (Pattern a) (Expression a)
                  | Index a (Name a) (Expression a)
                  | Access a (Expression a) (Name a)
                  | StringLit String
                  | CharLit Char
                  | AddrAt a (Expression a)
                  | ViewAt a (Expression a)
                  | Binary (BinOp a) (Expression a) (Expression a)
                  | Unary (UnOp a) (Expression a)
                  | IfCase { posE   :: a
                           , ifArms :: [(Expression a, LambdaType a, Expression a)] -- TODO I'm not sure @ifcase@ needs 'LambdaType'?
                           }
                  | Case { posE  :: a
                         , kind  :: Addendum
                         , val   :: Expression a
                         , _arms :: [(Pattern a, LambdaType a, Expression a)] -- ^ Each (('Pattern' a), ('Expression' a)) pair corresponds to a branch of the 'case' statement
                         }
                  | RecordValue a [(String, Expression a)] (Maybe (Type a))
                  | Precede (Expression a) (Expression a)
                  | ProofExpr a (Expression a) (Expression a)
                  | TypeSignature (Expression a) (Type a)
                  | WhereExp (Expression a) (ATS a)
                  | TupleEx a [Expression a]
                  | BoxTupleEx a [Expression a]
                  | While a (Expression a) (Expression a)
                  | Actions (ATS a)
                  | Begin a (Expression a)
                  | BinList { _op :: BinOp a, _exprs :: [Expression a] }
                  | PrecedeList { _exprs :: [Expression a] }
                  | FixAt a String (StackFunction a)
                  | LambdaAt a (StackFunction a)
                  | ParenExpr a (Expression a)
                  | CommentExpr String (Expression a)
                  | MacroVar a String
                  deriving (Show, Eq, Generic, NFData)

-- | An 'implement' or 'primplmnt' declaration
data Implementation a = Implement { pos            :: a
                                  , preUniversalsI :: [Universal a]
                                  , implicits      :: [[Type a]] -- ^ Implicit arguments
                                  , universalsI    :: [Universal a] -- ^ Universal quantifiers
                                  , nameI          :: Name a -- ^ ('Name' a) of the template being implemented
                                  , iArgs          :: [Arg a] -- ^ Arguments
                                  , _iExpression   :: Either (StaticExpression a) (Expression a) -- ^ Expression (or static expression) holding the function body.
                                  }
    deriving (Show, Eq, Generic, NFData)

-- | A function declaration accounting for all keywords ATS uses to
-- define them.
data Function a = Fun { _preF :: PreFunction a }
                | Fn { _preF :: PreFunction a }
                | Fnx { _preF :: PreFunction a }
                | And { _preF :: PreFunction a }
                | PrFun { _preF :: PreFunction a }
                | PrFn { _preF :: PreFunction a }
                | Praxi { _preF :: PreFunction a }
                | CastFn { _preF :: PreFunction a }
                deriving (Show, Eq, Generic, NFData)

-- | A type for stack-allocated functions. See
-- [here](http://ats-lang.sourceforge.net/DOCUMENT/ATS2TUTORIAL/HTML/c1267.html)
-- for more.
data StackFunction a = StackF { stSig        :: String
                              , stArgs       :: [Arg a]
                              , stReturnType :: Type a
                              , stExpression :: Expression a
                              }
                              deriving (Show, Eq, Generic, NFData)

data PreFunction a = PreF { fname         :: Name a -- ^ Function name
                          , sig           :: Maybe String -- ^ e.g. <> or \<!wrt>
                          , preUniversals :: [Universal a] -- ^ Universal quantifiers making a function generic
                          , universals    :: [Universal a] -- ^ (Universal a) quantifiers/refinement type
                          , args          :: [Arg a] -- ^ Actual function arguments
                          , returnType    :: Maybe (Type a) -- ^ Return type
                          , termetric     :: Maybe (StaticExpression a) -- ^ Optional termination metric
                          , _expression   :: Maybe (Expression a) -- ^ Expression holding the actual function body (not present in static templates)
                          }
                          deriving (Show, Eq, Generic, NFData)

join <$> traverse makeBaseFunctor [''Pattern, ''Expression, ''StaticExpression, ''Type]
join <$> traverse makeLenses [''Leaf, ''Declaration, ''PreFunction, ''Implementation, ''DataPropLeaf, ''Function, ''Type]

exprLens :: Eq a => FixityState a -> ASetter s t (Expression a) (Expression a) -> s -> t
exprLens st = flip over (rewriteATS st)

exprLenses :: Eq a => FixityState a -> [ASetter b b (Expression a) (Expression a)] -> b -> b
exprLenses st = thread . fmap (exprLens st)

rewriteDecl :: Eq a => FixityState a -> Declaration a -> Declaration a
rewriteDecl st (Extern l d) = Extern l (rewriteDecl st d)
rewriteDecl st x@Val{} = exprLens st valExpression x
rewriteDecl st x@Var{} = exprLenses st [varExpr1._Just, varExpr2._Just] x
rewriteDecl st x@Func{} = exprLens st (fun.preF.expression._Just) x
rewriteDecl st x@Impl{} = exprLens st (impl.iExpression._Right) x
rewriteDecl st x@PrVal{} = exprLens st (prValExpr._Just) x
rewriteDecl st x@AndDecl{} = exprLens st andExpr x
rewriteDecl st x@DataProp{} = exprLenses st (fmap ((propLeaves.each).) [propExpr1, propExpr2._Just]) x
rewriteDecl _ x@SumViewType{} = g x
    where g = over (leaves.mapped.constructorUniversals) h
          h :: Eq a => [Universal a] -> [Universal a]
          h = ana c where
            c (y:y':ys)
                | typeU y == typeU y' && isJust (typeU y) =
                    Cons (Universal (bound y ++ bound y') (typeU y) (StaticBinary LogicalAnd <$> prop y <*> prop y')) ys
            c y = project y
rewriteDecl _ x = x

-- FIXME left vs. right shouldn't be treated the same
instance (Eq a) => Ord (Fixity a) where
    compare = on compare ifix

leftFix :: Int -> Fixity a
leftFix = LeftFix undefined . Left

rightFix :: Int -> Fixity a
rightFix = RightFix undefined . Left

infix_ :: Int -> Fixity a
infix_ = Infix undefined . Left

type FixityState a = M.Map String (Fixity a)

-- | Fixities for operators in the ATS prelude.
defaultFixityState :: FixityState a
defaultFixityState = M.fromList
    [ ("::", rightFix 40) ]

-- | Default fixities from @fixity.ats@
getFixity :: FixityState a -> BinOp a -> Fixity a
getFixity _ Add                   = leftFix 50
getFixity _ Sub                   = leftFix 50
getFixity _ Mutate                = infix_ 0
getFixity _ Mult                  = leftFix 60
getFixity _ Div                   = leftFix 60
getFixity _ SpearOp               = rightFix 10
getFixity _ LogicalAnd            = leftFix 21
getFixity _ LogicalOr             = leftFix 20
getFixity _ At                    = rightFix 40
getFixity _ GreaterThan           = infix_ 40
getFixity _ GreaterThanEq         = infix_ 40
getFixity _ LessThanEq            = infix_ 40
getFixity _ Equal                 = infix_ 30
getFixity _ NotEq                 = infix_ 30
getFixity _ StaticEq              = infix_ 30
getFixity _ Mod                   = leftFix 60
getFixity _ LessThan              = infix_ 40
getFixity st (SpecialInfix _ op') =
    case M.lookup op' st of
        (Just f) -> f
        Nothing  -> infix_ 100

-- FIXME this should account for right vs. left associativity.
compareFixity :: Eq a => FixityState a -> BinOp a -> BinOp a -> Bool
compareFixity st = (== GT) .* on compare (getFixity st)

-- | Among other things, this rewrites expressions so that operator precedence
-- is respected; this ensures @1 + 2 * 3@ will be parsed as the correct
-- expression.
rewriteATS :: Eq a => FixityState a -> Expression a -> Expression a
rewriteATS st = cata a where
    a (LetF loc (ATS ds) e')                         = Let loc (ATS (rewriteDecl st <$> ds)) e'
    a (CallF n ts ts' me [ParenExpr _ e@NamedVal{}]) = Call n ts ts' me [e]
    a (CallF n ts ts' me [ParenExpr _ e@Call{}])     = Call n ts ts' me [e]
    a (PrecedeF e e'@PrecedeList{})                  = PrecedeList (e : _exprs e')
    a (PrecedeF e e')                                = PrecedeList [e, e']
    a (CallF n _ _ _ [Unary (SpecialOp loc s) e])    = Binary (SpecialInfix loc s) (NamedVal n) e
    a (BinaryF op' (Binary op'' e e') e'')
        | compareFixity st op' op'' = Binary op'' e (Binary op' e' e'')
    a (BinaryF Add e (BinList Add es))               = BinList Add (e : es)
    a (BinaryF Add e e')                             = BinList Add [e, e']
    a (BinaryF Con{} e (BinList Add es))             = BinList (SpecialInfix undefined "::") (e : es)
    a (BinaryF Con{} e e')                           = BinList (SpecialInfix undefined "::") [e, e']
    a (ParenExprF _ e@Precede{})                     = e
    a (ParenExprF _ e@PrecedeList{})                 = e
    a (WhereExpF e (ATS ds))                         = WhereExp e (ATS (rewriteDecl st <$> ds))
    a (ActionsF (ATS ds))                            = Actions (ATS (rewriteDecl st <$> ds))
    a x                                              = embed x
