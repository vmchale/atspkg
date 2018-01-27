{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Error ( PackageError (..)
                                  , printErr
                                  -- * Helper functions
                                  , unrecognized
                                  ) where

import           System.Exit
import           Text.PrettyPrint.ANSI.Leijen

infixr 5 <#>

unrecognized :: String -> IO a
unrecognized = printErr . Unrecognized

newtype PackageError = Unrecognized String

(<#>) :: Doc -> Doc -> Doc
(<#>) a b = a <> line <> b

instance Pretty PackageError where
    pretty (Unrecognized t) = red "Error:" <> "Unrecognized archive format when unpacking" <#> hang 2 (text t)

-- TODO monaderror?
printErr :: PackageError -> IO a
printErr e = putDoc (pretty e) >> exitFailure
