module Language.ATS.Rewrite ( rewriteDecl
                            -- * Fixity
                            , defaultFixityState
                            ) where

import           Control.Composition
import           Data.Function           (on)
import           Data.Functor.Foldable
import qualified Data.Map                as M
import           Data.Maybe              (isJust)
import           Language.ATS.Types
import           Language.ATS.Types.Lens
import           Lens.Micro

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

-- | Fixities for operators in the ATS prelude.
defaultFixityState :: FixityState a
defaultFixityState = M.fromList
    [ ("::", rightFix 40) ]

leftFix :: Int -> Fixity a
leftFix = LeftFix undefined . Left

rightFix :: Int -> Fixity a
rightFix = RightFix undefined . Left

infix_ :: Int -> Fixity a
infix_ = Infix undefined . Left

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
