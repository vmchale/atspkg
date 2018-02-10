{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Error ( -- * Helper functions
                                    unrecognized
                                  , resolutionFailed
                                  ) where

import           Data.Dependency
import           System.Exit
import           Text.PrettyPrint.ANSI.Leijen

infixr 5 <#>

unrecognized :: String -> IO a
unrecognized = printErr . Unrecognized

resolutionFailed :: ResolveError -> IO a
resolutionFailed = printErr . DepErr

data PackageError = Unrecognized String
                  | DepErr ResolveError

(<#>) :: Doc -> Doc -> Doc
(<#>) a b = a <> line <> b

instance Pretty PackageError where
    pretty (Unrecognized t) = red "Error:" <+> "Unrecognized archive format when unpacking" <#> hang 2 (text t)
    pretty (DepErr d)       = pretty d

-- TODO monaderror?
printErr :: PackageError -> IO a
printErr e = putDoc (pretty e) >> exitFailure
