module Language.ATS.Package.Dhall ( checkPkgSet
                                  , checkPkg
                                  ) where

import           Data.Dependency
import           Language.ATS.Package.PackageSet
import           Language.ATS.Package.Type
import           Quaalude

checkPkg :: FilePath
         -> Bool
         -> IO (Version -> ATSDependency)
checkPkg = checkDhall

checkDhall :: Interpret a
      => FilePath
      -> Bool
      -> IO a
checkDhall path d =
    bool id detailed d $ input auto (pack ('.' : '/' : path))

checkPkgSet :: FilePath -- ^ Path to @.dhall@ file defining a package set.
            -> Bool -- ^ Whether to print detailed error messages.
            -> IO ATSPackageSet
checkPkgSet = checkDhall
