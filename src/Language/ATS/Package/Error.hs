{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Error ( -- * Helper functions
                                    unrecognized
                                  , resolutionFailed
                                  ) where

import           System.Exit
import           Text.PrettyPrint.ANSI.Leijen

infixr 5 <#>

unrecognized :: String -> IO a
unrecognized = printErr . Unrecognized

resolutionFailed :: IO a
resolutionFailed = printErr ResolutionFailed

data PackageError = Unrecognized String
                     | ResolutionFailed

(<#>) :: Doc -> Doc -> Doc
(<#>) a b = a <> line <> b

instance Pretty PackageError where
    pretty (Unrecognized t) = red "Error:" <> "Unrecognized archive format when unpacking" <#> hang 2 (text t)
    pretty ResolutionFailed = red "Error:" <> "Package resolution failed."

-- TODO monaderror?
printErr :: PackageError -> IO a
printErr e = putDoc (pretty e) >> exitFailure
