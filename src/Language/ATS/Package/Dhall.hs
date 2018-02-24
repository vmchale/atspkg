module Language.ATS.Package.Dhall ( checkPkg
                                  ) where

import           Language.ATS.Package.PackageSet
import           Quaalude

checkPkg :: FilePath -- ^ Path to @pkg.dhall@ or similar.
         -> Bool -- ^ Whether to print detailed error messages.
         -> IO ATSPackageSet
checkPkg path d = do
    x <- input auto (pack ('.' : '/' : path))
    let f = bool id detailed d
    f (pure (x :: ATSPackageSet))
