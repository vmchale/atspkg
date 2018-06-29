module Language.ATS.Package.Dhall ( checkPkgSet
                                  , checkPkg
                                  ) where

import           Data.Dependency
import qualified Data.Text                       as T
import           Language.ATS.Package.PackageSet
import           Language.ATS.Package.Type
import           Quaalude

-- | Check a @pkg.dhall@ file.
checkPkg :: FilePath
         -> Bool
         -> IO (Version -> ATSDependency)
checkPkg = checkDhall

checkDhall :: Interpret a
           => FilePath
           -> Bool
           -> IO a
checkDhall path d =
    bool id detailed d $
        input auto (T.pack ('.' : pathSeparator : path))

checkPkgSet :: FilePath -- ^ Path to @.dhall@ file defining a package set.
            -> Bool -- ^ Whether to print detailed error messages.
            -> IO ATSPackageSet
checkPkgSet = checkDhall
