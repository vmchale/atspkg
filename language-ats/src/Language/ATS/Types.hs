{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
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
    , SortF (..)
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
import           Control.DeepSeq       (NFData)
import           Data.Function         (on)
import           Data.Functor.Foldable hiding (Fix (..))
import qualified Data.Map              as M
import           Data.Maybe            (isJust)
import           Data.Semigroup        (Semigroup)
import           GHC.Generics          (Generic)
import           Language.ATS.Lexer    (Addendum (..))
import           Lens.Micro

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

constructorUniversals :: Lens' (Leaf a) [Universal a]
constructorUniversals f s = fmap (\x -> s { _constructorUniversals = x}) (f (_constructorUniversals s))

type SortArgs a = Maybe [SortArg a]

fun :: Traversal' (Declaration a) (Function a)
fun f (Func x y) = Func x <$> f y
fun _ x          = pure x

impl :: Traversal' (Declaration a) (Implementation a)
impl f (Impl x y)      = Impl x <$> f y
impl f (ProofImpl x y) = ProofImpl x <$> f y
impl _ x               = pure x

valExpression :: Traversal' (Declaration a) (Expression a)
valExpression f (Val a v p e) = Val a v p <$> f e
valExpression _ x             = pure x

prValExpr :: Traversal' (Declaration a) (Maybe (Expression a))
prValExpr f (PrVal p me t) = (\me' -> PrVal p me' t) <$> f me
prValExpr _ x              = pure x

varExpr1 :: Traversal' (Declaration a) (Maybe (Expression a))
varExpr1 f (Var t p e e') = (\e'' -> Var t p e'' e') <$> f e
varExpr1 _ x              = pure x

varExpr2 :: Traversal' (Declaration a) (Maybe (Expression a))
varExpr2 f (Var t p e e') = (\e'' -> Var t p e e'') <$> f e'
varExpr2 _ x              = pure x

andExpr :: Traversal' (Declaration a) (Expression a)
andExpr f (AndDecl t p e) = AndDecl t p <$> f e
andExpr _ x               = pure x

propLeaves :: Traversal' (Declaration a) [DataPropLeaf a]
propLeaves f (DataProp l n as pl) = DataProp l n as <$> f pl
propLeaves _ x                    = pure x

leaves :: Traversal' (Declaration a) [Leaf a]
leaves f (SumType t as l)     = SumType t as <$> f l
leaves f (SumViewType t as l) = SumViewType t as <$> f l
leaves _ x                    = pure x

comment :: Traversal' (Declaration a) String
comment f (Comment c) = Comment <$> f c
comment _ x           = pure x

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
                   | Assume (Name a) (SortArgs a) (Type a)
                   | TKind a (Name a) String
                   | SymIntr a [Name a]
                   | Stacst a (Name a) (Type a) (Maybe (Expression a))
                   | PropDef a String [Arg a] (Type a)
                   | FixityDecl (Fixity a) [String]
                   | MacDecl a String [String] (Expression a)
                   | DataSort a String [DataSortLeaf a]
                   | Exception String (Type a)
                   | ExtVar a String (Expression a)
                   | AbsImpl a (Name a) (SortArgs a) (Type a)
                   deriving (Show, Eq, Generic, NFData)

data DataSortLeaf a = DataSortLeaf [Universal a] (Sort a) (Maybe (Sort a))
                    deriving (Show, Eq, Generic, NFData)

data DataPropLeaf a = DataPropLeaf { propU :: [Universal a], _propExpr1 :: Expression a, _propExpr2 :: Maybe (Expression a) }
                    deriving (Show, Eq, Generic, NFData)

propExpr1 :: Lens' (DataPropLeaf a) (Expression a)
propExpr1 f s = fmap (\x -> s { _propExpr1 = x}) (f (_propExpr1 s))

propExpr2 :: Lens' (DataPropLeaf a) (Maybe (Expression a))
propExpr2 f s = fmap (\x -> s { _propExpr2 = x}) (f (_propExpr2 s))

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

data TypeF a x = TupleF a [x]
               | BoxTupleF a [x]
               | NamedF (Name a)
               | ExF (Existential a) (Maybe x)
               | ForAF (Universal a) x
               | DependentF (Name a) [x]
               | UnconsumedF x
               | AsProofF x (Maybe x)
               | FromVTF x
               | MaybeValF x
               | AtExprF a x (StaticExpression a)
               | AtTypeF a x
               | ProofTypeF a [x] x
               | ConcreteTypeF (StaticExpression a)
               | RefTypeF x
               | ViewTypeF a x
               | FunctionTypeF String x x
               | NoneTypeF a
               | ImplicitTypeF a
               | ViewLiteralF Addendum
               | AnonymousRecordF a [(String, x)]
               | ParenTypeF a x
               | WhereTypeF a x String (SortArgs a) x
               deriving (Functor)

type instance Base (Type a) = TypeF a

instance Recursive (Type a) where
    project (Tuple x tys)              = TupleF x tys
    project (BoxTuple x tys)           = BoxTupleF x tys
    project (Named n)                  = NamedF n
    project (Ex e mty)                 = ExF e mty
    project (ForA u ty)                = ForAF u ty
    project (Dependent n tys)          = DependentF n tys
    project (Unconsumed ty)            = UnconsumedF ty
    project (AsProof ty mty)           = AsProofF ty mty
    project (FromVT ty)                = FromVTF ty
    project (MaybeVal ty)              = MaybeValF ty
    project (AtExpr l ty se)           = AtExprF l ty se
    project (AtType l ty)              = AtTypeF l ty
    project (ProofType l tys ty')      = ProofTypeF l tys ty'
    project (ConcreteType se)          = ConcreteTypeF se
    project (RefType ty)               = RefTypeF ty
    project (ViewType l ty)            = ViewTypeF l ty
    project (FunctionType s ty ty')    = FunctionTypeF s ty ty'
    project (NoneType l)               = NoneTypeF l
    project (ImplicitType l)           = ImplicitTypeF l
    project (ViewLiteral a)            = ViewLiteralF a
    project (AnonymousRecord l stys)   = AnonymousRecordF l stys
    project (ParenType l ty)           = ParenTypeF l ty
    project (WhereType l ty s sas ty') = WhereTypeF l ty s sas ty'

typeCall :: Traversal' (Type a) (Name a)
typeCall f (Dependent x y) = (\x' -> Dependent x' y) <$> f x
typeCall _ x               = pure x

typeCallArgs :: Traversal' (Type a) [Type a]
typeCallArgs f (Dependent x y) = (\y' -> Dependent x y') <$> f y
typeCallArgs _ x               = pure x

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
               | As a (Pattern a) (Pattern a)
               deriving (Show, Eq, Generic, NFData)

data PatternF a x = WildcardF a
                  | PNameF (Name a) [x]
                  | PSumF String x
                  | PLiteralF (Expression a)
                  | GuardedF a (Expression a) x
                  | FreeF x
                  | ProofF a [x] [x]
                  | TuplePatternF [x]
                  | BoxTuplePatternF a [x]
                  | AtPatternF a x
                  | UniversalPatternF a String [Universal a] x
                  | ExistentialPatternF (Existential a) x
                  | AsF a x x
                  deriving (Functor)

type instance Base (Pattern a) = PatternF a

instance Recursive (Pattern a) where
    project (Wildcard a)                = WildcardF a
    project (PName x ps)                = PNameF x ps
    project (PSum x p)                  = PSumF x p
    project (PLiteral e)                = PLiteralF e
    project (Guarded x e p)             = GuardedF x e p
    project (Free p)                    = FreeF p
    project (Proof x ps ps')            = ProofF x ps ps'
    project (TuplePattern ps)           = TuplePatternF ps
    project (BoxTuplePattern x ps)      = BoxTuplePatternF x ps
    project (AtPattern x p)             = AtPatternF x p
    project (UniversalPattern x s us p) = UniversalPatternF x s us p
    project (ExistentialPattern e x)    = ExistentialPatternF e x
    project (As x p p')                 = AsF x p p'

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
            | ArrowSort a (Sort a) (Sort a)
            deriving (Show, Eq, Generic, NFData)

data SortF a x = NamedSortF String
               | T0pF Addendum
               | Vt0pF Addendum
               | AddrF
               | VTypeF a Addendum
               | ViewF a Addendum
               | TupleSortF a x x
               | ArrowSortF a x x
               deriving (Functor)

type instance Base (Sort a) = SortF a

instance Recursive (Sort a) where
    project (NamedSort s)      = NamedSortF s
    project (T0p a)            = T0pF a
    project (Vt0p a)           = Vt0pF a
    project Addr               = AddrF
    project (VType x a)        = VTypeF x a
    project (View x a)         = ViewF x a
    project (TupleSort x s s') = TupleSortF x s s'
    project (ArrowSort x s s') = ArrowSortF x s s'

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
                        | Sif { scond :: StaticExpression a, whenTrue :: StaticExpression a, selseExpr :: StaticExpression a } -- Static if (for proofs)
                        | SCall (Name a) [StaticExpression a]
                        | SUnary (UnOp a) (StaticExpression a)
                        | SLet a [Declaration a] (Maybe (StaticExpression a))
                        | SCase Addendum (StaticExpression a) [(Pattern a, LambdaType a, StaticExpression a)]
                        deriving (Show, Eq, Generic, NFData)

data StaticExpressionF a x = StaticValF (Name a)
                           | StaticBinaryF (BinOp a) x x
                           | StaticIntF Int
                           | SPrecedeF x x
                           | StaticVoidF a
                           | SifF x x x
                           | SCallF (Name a) [x]
                           | SUnaryF (UnOp a) x
                           | SLetF a [Declaration a] (Maybe x)
                           | SCaseF Addendum x [(Pattern a, LambdaType a, x)]
                           deriving (Functor)

type instance Base (StaticExpression a) = StaticExpressionF a

instance Recursive (StaticExpression a) where
    project (StaticVal n)         = StaticValF n
    project (StaticBinary b x x') = StaticBinaryF b x x'
    project (StaticInt i)         = StaticIntF i
    project (SPrecede x x')       = SPrecedeF x x'
    project (StaticVoid x)        = StaticVoidF x
    project (Sif e e' e'')        = SifF e e' e''
    project (SCall n es)          = SCallF n es
    project (SUnary u x)          = SUnaryF u x
    project (SLet x ds e)         = SLetF x ds e
    project (SCase a x ples)      = SCaseF a x ples

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
                  | HexLit String
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

data ExpressionF a x = LetF a (ATS a) (Maybe x)
                     | VoidLiteralF a
                     | CallF (Name a) [[Type a]] [Type a] (Maybe [x]) [x]
                     | NamedValF (Name a)
                     | ListLiteralF a String (Type a) [x]
                     | IfF x x (Maybe x)
                     | UintLitF Word
                     | FloatLitF Float
                     | IntLitF Int
                     | HexLitF String
                     | UnderscoreLitF a
                     | LambdaF a (LambdaType a) (Pattern a) x
                     | LinearLambdaF a (LambdaType a) (Pattern a) x
                     | IndexF a (Name a) x
                     | AccessF a x (Name a)
                     | StringLitF String
                     | CharLitF Char
                     | AddrAtF a x
                     | ViewAtF a x
                     | BinaryF (BinOp a) x x
                     | UnaryF (UnOp a) x
                     | IfCaseF a [(x, LambdaType a, x)]
                     | CaseF a Addendum x [(Pattern a, LambdaType a, x)]
                     | RecordValueF a [(String, x)] (Maybe (Type a))
                     | PrecedeF x x
                     | ProofExprF a x x
                     | TypeSignatureF x (Type a)
                     | WhereExpF x (ATS a)
                     | TupleExF a [x]
                     | BoxTupleExF a [x]
                     | WhileF a x x
                     | ActionsF (ATS a)
                     | BeginF a x
                     | BinListF (BinOp a) [x]
                     | PrecedeListF [x]
                     | FixAtF a String (StackFunction a)
                     | LambdaAtF a (StackFunction a)
                     | ParenExprF a x
                     | CommentExprF String x
                     | MacroVarF a String
                     deriving (Functor)

type instance Base (Expression a) = ExpressionF a

instance Recursive (Expression a) where
    project (Let l ds me)            = LetF l ds me
    project (VoidLiteral l)          = VoidLiteralF l
    project (Call n is us mps as)    = CallF n is us mps as
    project (NamedVal n)             = NamedValF n
    project (ListLiteral l s t es)   = ListLiteralF l s t es
    project (If e e' me)             = IfF e e' me
    project (UintLit u)              = UintLitF u
    project (FloatLit f)             = FloatLitF f
    project (IntLit i)               = IntLitF i
    project (HexLit s)               = HexLitF s
    project (UnderscoreLit l)        = UnderscoreLitF l
    project (Lambda l lt p e)        = LambdaF l lt p e
    project (LinearLambda l lt p e)  = LinearLambdaF l lt p e
    project (Index l n e)            = IndexF l n e
    project (Access l e n)           = AccessF l e n
    project (StringLit s)            = StringLitF s
    project (CharLit c)              = CharLitF c
    project (AddrAt l e)             = AddrAtF l e
    project (ViewAt l e)             = ViewAtF l e
    project (Binary op e e')         = BinaryF op e e'
    project (Unary op e)             = UnaryF op e
    project (IfCase l arms)          = IfCaseF l arms
    project (Case l k e arms)        = CaseF l k e arms
    project (RecordValue l recs mty) = RecordValueF l recs mty
    project (Precede e e')           = PrecedeF e e'
    project (ProofExpr a e e')       = ProofExprF a e e'
    project (TypeSignature e ty)     = TypeSignatureF e ty
    project (WhereExp e ds)          = WhereExpF e ds
    project (TupleEx l es)           = TupleExF l es
    project (BoxTupleEx l es)        = BoxTupleExF l es
    project (While l e e')           = WhileF l e e'
    project (Actions ds)             = ActionsF ds
    project (Begin l e)              = BeginF l e
    project (BinList op es)          = BinListF op es
    project (PrecedeList es)         = PrecedeListF es
    project (FixAt a s sfun)         = FixAtF a s sfun
    project (LambdaAt a sfun)        = LambdaAtF a sfun
    project (ParenExpr l e)          = ParenExprF l e
    project (CommentExpr s e)        = CommentExprF s e
    project (MacroVar l s)           = MacroVarF l s

instance Corecursive (Expression a) where
    embed (LetF l ds me)            = Let l ds me
    embed (VoidLiteralF l)          = VoidLiteral l
    embed (CallF n is us mps as)    = Call n is us mps as
    embed (NamedValF n)             = NamedVal n
    embed (ListLiteralF l s t es)   = ListLiteral l s t es
    embed (IfF e e' me)             = If e e' me
    embed (UintLitF u)              = UintLit u
    embed (IntLitF i)               = IntLit i
    embed (FloatLitF f)             = FloatLit f
    embed (HexLitF s)               = HexLit s
    embed (UnderscoreLitF l)        = UnderscoreLit l
    embed (LambdaF l lt p e)        = Lambda l lt p e
    embed (LinearLambdaF l lt p e)  = LinearLambda l lt p e
    embed (IndexF l n e)            = Index l n e
    embed (AccessF l n e)           = Access l n e
    embed (StringLitF s)            = StringLit s
    embed (CharLitF c)              = CharLit c
    embed (AddrAtF l e)             = AddrAt l e
    embed (ViewAtF l e)             = ViewAt l e
    embed (BinaryF op e e')         = Binary op e e'
    embed (UnaryF op e)             = Unary op e
    embed (IfCaseF l arms)          = IfCase l arms
    embed (CaseF l k e arms)        = Case l k e arms
    embed (RecordValueF l recs mty) = RecordValue l recs mty
    embed (PrecedeF e e')           = Precede e e'
    embed (ProofExprF a e e')       = ProofExpr a e e'
    embed (TypeSignatureF e ty)     = TypeSignature e ty
    embed (WhereExpF e ds)          = WhereExp e ds
    embed (TupleExF l es)           = TupleEx l es
    embed (BoxTupleExF l es)        = BoxTupleEx l es
    embed (WhileF l e e')           = While l e e'
    embed (ActionsF ds)             = Actions ds
    embed (BeginF l e)              = Begin l e
    embed (BinListF op es)          = BinList op es
    embed (PrecedeListF es)         = PrecedeList es
    embed (FixAtF a s sfun)         = FixAt a s sfun
    embed (LambdaAtF a sfun)        = LambdaAt a sfun
    embed (ParenExprF l e)          = ParenExpr l e
    embed (CommentExprF s e)        = CommentExpr s e
    embed (MacroVarF l s)           = MacroVar l s

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

iExpression :: Lens' (Implementation a) (Either (StaticExpression a) (Expression a))
iExpression f s = fmap (\x -> s { _iExpression = x}) (f (_iExpression s))

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

preF :: Lens' (Function a) (PreFunction a)
preF f s = fmap (\x -> s { _preF = x}) (f (_preF s))

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

-- TODO lens/base functor module?
expression :: Lens' (PreFunction a) (Maybe (Expression a))
expression f s = fmap (\x -> s { _expression = x}) (f (_expression s))

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
