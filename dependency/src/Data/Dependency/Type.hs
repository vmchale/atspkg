{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Dependency.Type ( Dependency (..)
                            , Version (..)
                            , Constraint (..)
                            , PackageSet (..)
                            , ResMap
                            , ResolveStateM (..)
                            , ResolveState
                            -- * Helper functions
                            , satisfies
                            , compatible
                            ) where

import           Control.DeepSeq              (NFData)
import           Control.Monad.Fix
import           Control.Monad.Tardis
import           Control.Monad.Trans.Class
import           Data.Binary
import           Data.Dependency.Error
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List                    (intercalate)
import qualified Data.Map                     as M
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import qualified Data.Set                     as S
import           GHC.Generics                 (Generic)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

type Mod = ResMap -> ResMap
type ResMap = M.Map String Dependency
type ResolveState = ResolveStateM (Either ResolveError)

newtype ResolveStateM m a = ResolveStateM { unResolve :: TardisT Mod ResMap m a }
    deriving (Functor)
    deriving newtype (Applicative, Monad, MonadFix, MonadTardis Mod ResMap)

instance MonadTrans ResolveStateM where
    lift = ResolveStateM . lift

newtype PackageSet a = PackageSet (M.Map String (S.Set a))
    deriving (Eq, Ord, Foldable, Generic)
    deriving newtype Binary

newtype Version = Version [Integer]
    deriving (Eq, Generic)
    deriving newtype (NFData, Binary)

{-# COMPLETE V #-}

pattern V :: [Integer] -> Version
pattern V is = Version is

instance Show Version where
    show (Version is) = intercalate "." (show <$> is)

instance Ord Version where
    (V []) <= (V []) = True
    (V []) <= _ = True
    _ <= (V []) = False
    (V (x:xs)) <= (V (y:ys))
        | x == y = Version xs <= Version ys
        | otherwise = x <= y

-- TODO comonad??
data Constraint a = LessThanEq a
                  | GreaterThanEq a
                  | Eq a
                  | Bounded (Constraint a) (Constraint a)
                  | None
                  deriving (Show, Eq, Ord, Functor, Generic, NFData)

-- free moinoid but "pokey" because it has extra constructors
instance Semigroup (Constraint a) where
    (<>) None x = x
    (<>) x None = x
    (<>) x y    = Bounded x y

instance Monoid (Constraint a) where
    mempty = None
    mappend = (<>)

data Dependency = Dependency { _libName         :: String
                             , _libConstraint   :: Constraint Version
                             , _libDependencies :: [String]
                             , _libVersion      :: Version
                             }
                             deriving (Show, Eq, Ord, Generic, NFData)

satisfies :: (Ord a) => Constraint a -> a -> Bool
satisfies (LessThanEq x) y    = x <= y
satisfies (GreaterThanEq x) y = x >= y
satisfies (Eq x) y            = x == y
satisfies (Bounded x y) z     = satisfies x z && satisfies y z
satisfies None _              = True

compatible :: (Ord a) => Constraint a -> Constraint a -> Bool
compatible LessThanEq{} LessThanEq{}        = True
compatible (LessThanEq x) (GreaterThanEq y) = y <= x
compatible (Eq x) (Eq y)                    = x == y
compatible (Bounded x y) z                  = compatible x z && compatible y z
compatible GreaterThanEq{} GreaterThanEq{}  = True
compatible (LessThanEq x) (Eq y)            = y <= x
compatible None _                           = True
compatible x y                              = compatible y x

makeBaseFunctor ''Constraint

instance Pretty a => Pretty (Constraint a) where
    pretty = cata a where
        a (LessThanEqF v)    = "≤" <+> pretty v
        a (GreaterThanEqF v) = "≥" <+> pretty v
        a (EqF v)            = "≡" <+> pretty v
        a (BoundedF c c')    = c <+> "∧" <+> c'
        a NoneF              = mempty
