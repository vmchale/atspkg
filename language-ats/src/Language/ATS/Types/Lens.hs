module Language.ATS.Types.Lens ( -- * Lenses
                                 iExpression
                               , constructorUniversals
                               , expression
                               , preF
                               , propExpr1
                               , propExpr2
                               -- * Traversals
                               , typeCall
                               , typeCallArgs
                               , fun
                               , impl
                               , valExpression
                               , prValExpr
                               , varExpr1
                               , varExpr2
                               , andExpr
                               , propLeaves
                               , leaves
                               , comment
                               ) where

import           Language.ATS.Types
import           Lens.Micro

constructorUniversals :: Lens' (Leaf a) [Universal a]
constructorUniversals f s = fmap (\x -> s { _constructorUniversals = x}) (f (_constructorUniversals s))

expression :: Lens' (PreFunction a) (Maybe (Expression a))
expression f s = fmap (\x -> s { _expression = x}) (f (_expression s))

preF :: Lens' (Function a) (PreFunction a)
preF f s = fmap (\x -> s { _preF = x}) (f (_preF s))

iExpression :: Lens' (Implementation a) (Either (StaticExpression a) (Expression a))
iExpression f s = fmap (\x -> s { _iExpression = x}) (f (_iExpression s))

propExpr1 :: Lens' (DataPropLeaf a) (Expression a)
propExpr1 f s = fmap (\x -> s { _propExpr1 = x}) (f (_propExpr1 s))

propExpr2 :: Lens' (DataPropLeaf a) (Maybe (Expression a))
propExpr2 f s = fmap (\x -> s { _propExpr2 = x}) (f (_propExpr2 s))


typeCall :: Traversal' (Type a) (Name a)
typeCall f (Dependent x y) = (\x' -> Dependent x' y) <$> f x
typeCall _ x               = pure x

typeCallArgs :: Traversal' (Type a) [Type a]
typeCallArgs f (Dependent x y) = (\y' -> Dependent x y') <$> f y
typeCallArgs _ x               = pure x

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
