{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Dependency.Type ( Dependency (..)
                            , Version (..)
                            , Constraint (..)
                            , PackageSet (..)
                            -- * Helper functions
                            , check
                            ) where

import           Control.DeepSeq              (NFData)
import           Data.Binary
import           Data.Functor.Foldable
import           Data.List                    (intercalate)
import qualified Data.Map                     as M
import           Data.Semigroup
import qualified Data.Set                     as S
import           GHC.Generics                 (Generic)
import           GHC.Natural                  (Natural)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

-- | A package set is simply a map between package names and a set of packages.
newtype PackageSet a = PackageSet { _packageSet :: M.Map String (S.Set a) }
    deriving (Eq, Ord, Foldable, Generic, Binary)

newtype Version = Version [Natural]
    deriving (Eq, Generic, NFData, Binary)

instance Show Version where
    show (Version is) = intercalate "." (show <$> is)

instance Ord Version where
    (Version []) <= _ = True
    _ <= (Version []) = False
    (Version (x:xs)) <= (Version (y:ys))
        | x == y = Version xs <= Version ys
        | otherwise = x <= y

-- | Monoid/functor for representing constraints.
data Constraint a = LessThanEq a
                  | GreaterThanEq a
                  | Eq a
                  | Bounded (Constraint a) (Constraint a)
                  | None
                  deriving (Show, Eq, Ord, Functor, Generic, NFData)

instance Semigroup (Constraint a) where
    (<>) None x = x
    (<>) x None = x
    (<>) x y    = Bounded x y

instance Monoid (Constraint a) where
    mempty = None
    mappend = (<>)

-- | A generic dependency, consisting of a package name and version, as well as
-- dependency names and their constraints.
data Dependency = Dependency { _libName         :: String
                             , _libDependencies :: [(String, Constraint Version)]
                             , _libVersion      :: Version
                             }
                             deriving (Show, Eq, Ord, Generic, NFData)

-- | Check a given dependency is compatible with in-scope dependencies.
check :: Dependency -> [Dependency] -> Bool
check (Dependency ln _ v) ds = and [ g ds' | (Dependency _ ds' _) <- ds ]
    where g = all ((`satisfies` v) . snd) . filter ((== ln) . fst)

satisfies :: (Ord a) => Constraint a -> a -> Bool
satisfies (LessThanEq x) y    = x >= y
satisfies (GreaterThanEq x) y = x <= y
satisfies (Eq x) y            = x == y
satisfies (Bounded x y) z     = satisfies x z && satisfies y z
satisfies None _              = True

data ConstraintF a x = LessThanEqF a
                     | GreaterThanEqF a
                     | EqF a
                     | BoundedF x x
                     | NoneF
                     deriving (Functor)

type instance Base (Constraint a) = ConstraintF a

instance Recursive (Constraint a) where
    project (LessThanEq x)    = LessThanEqF x
    project (GreaterThanEq x) = GreaterThanEqF x
    project (Eq x)            = EqF x
    project (Bounded x y)     = BoundedF x y
    project None              = NoneF

instance Pretty a => Pretty (Constraint a) where
    pretty = cata a where
        a (LessThanEqF v)    = "≤" <+> pretty v
        a (GreaterThanEqF v) = "≥" <+> pretty v
        a (EqF v)            = "≡" <+> pretty v
        a (BoundedF c c')    = c <+> "∧" <+> c'
        a NoneF              = mempty
