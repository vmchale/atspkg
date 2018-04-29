{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
                            , check
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
import           Data.Semigroup
import qualified Data.Set                     as S
import           GHC.Generics                 (Generic)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

type Mod = ResMap -> ResMap
type ResMap = M.Map String Dependency
type ResolveState = ResolveStateM (Either ResolveError)

-- | We use a tardis monad here to give ourselves greater flexibility during
-- dependency solving.
newtype ResolveStateM m a = ResolveStateM { unResolve :: TardisT Mod ResMap m a }
    deriving (Functor)
    deriving newtype (Applicative, Monad, MonadFix, MonadTardis Mod ResMap)

instance MonadTrans ResolveStateM where
    lift = ResolveStateM . lift

-- | A package set is simply a map between package names and a set of packages.
newtype PackageSet a = PackageSet { _packageSet :: M.Map String (S.Set a) }
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

check' :: Dependency -> [Dependency] -> Bool
check' (Dependency _ lds _) ds =
    and [ compatible (Eq v') c | (Dependency ln _ v') <- ds, (str, c) <- lds, ln == str ]

-- | Check a given dependency is compatible with in-scope dependencies.
check :: Dependency -> [Dependency] -> Bool
check d = (&&) <$> check' d <*> check'' d

check'' :: Dependency -> [Dependency] -> Bool
check'' (Dependency ln _ v) ds = and [ g ds' | (Dependency _ ds' _) <- ds ]
    where g = all ((`satisfies` v) . snd) . filter ((== ln) . fst)

satisfies :: (Ord a) => Constraint a -> a -> Bool
satisfies (LessThanEq x) y    = x >= y
satisfies (GreaterThanEq x) y = x <= y
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
