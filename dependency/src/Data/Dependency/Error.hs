{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Dependency.Error ( ResolveError (..)
                             , DepM
                             ) where

import           Control.DeepSeq
import           Data.Foldable                (fold)
import           GHC.Generics                 (Generic)
import           Text.PrettyPrint.ANSI.Leijen

type DepM = Either ResolveError

-- | An error that can occur during package resolution.
data ResolveError = InternalError
                  | NotPresent String
                  | Conflicts [String] String
                  | CircularDependencies String String
                  deriving (Show, Eq, NFData, Generic)

instance Pretty ResolveError where
    pretty InternalError = red "Error:" <+> "the" <+> squotes "dependency" <+> "package enountered an internal error. Please report this as a bug:\n" <> hang 2 "https://hub.darcs.net/vmchale/ats/issues"
    pretty (NotPresent s) = red "Error:" <+> "the package" <+> squotes (text s) <+> "could not be found.\n"
    pretty (Conflicts ss s) = red "Error:" <+> "the package" <+> squotes (text s) <+> "conflicts with already in-scope packages: " <+> fold (punctuate ", " (fmap (squotes . text) ss))
    pretty (CircularDependencies s s') = red "Error:" <+> "package" <+> squotes (text s) <+> "and package" <+> squotes (text s') <+> "form a cycle of dependencies"
