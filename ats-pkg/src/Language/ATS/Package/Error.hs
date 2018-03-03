{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Error ( -- * Helper functions
                                    unrecognized
                                  , resolutionFailed
                                  -- * Types
                                  , PackageError (..)
                                  ) where

import           Data.Dependency
import           Quaalude
import           System.Exit

unrecognized :: String -> IO a
unrecognized = printErr . Unrecognized

resolutionFailed :: ResolveError -> IO a
resolutionFailed = printErr . DepErr

data PackageError = Unrecognized String
                  | DepErr ResolveError

instance Pretty PackageError where
    pretty (Unrecognized t) = dullred "Error:" <+> "Unrecognized archive format when unpacking" <#> hang 2 (text t)
    pretty (DepErr d)       = pretty d

-- TODO monaderror?
printErr :: PackageError -> IO a
printErr e = putDoc (pretty e) >> exitFailure
