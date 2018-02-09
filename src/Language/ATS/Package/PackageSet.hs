{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Language.ATS.Package.PackageSet ( ATSPackageSet
                                       , setBuildPlan
                                       , mkBuildPlan
                                       ) where

import           Data.Binary                (decode)
import           Data.Bool                  (bool)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Dependency
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text.Lazy             as TL
import           Dhall                      hiding (bool)
import           Language.ATS.Package.Error
import           Language.ATS.Package.Type
import           System.Directory           (doesFileExist)

newtype ATSPackageSet = ATSPackageSet [ ATSDependency ]
    deriving (Interpret)

setBuildPlan :: FilePath -> [ATSDependency] -> IO [[ATSDependency]]
setBuildPlan p deps = do
    b <- doesFileExist depCache
    bool setBuildPlan' (decode <$> BSL.readFile depCache) b

    where depCache = ".atspkg/" ++ p
          setBuildPlan' = do
            pkgSet <- input auto "https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/pkg-set.dhall"
            case mkBuildPlan pkgSet deps of
                Just x  -> pure x
                Nothing -> resolutionFailed

mkBuildPlan :: ATSPackageSet -> [ATSDependency] -> Maybe [[ATSDependency]]
mkBuildPlan aps = finalize . resolve . buildSequence . fmap asDep
    where finalize = fmap (fmap (fmap (lookupVersions aps)))
          resolve = resolveDependencies (atsPkgsToPkgs aps)

asDep :: ATSDependency -> Dependency
asDep ATSDependency{..} = Dependency (TL.unpack libName) mempty mempty

atsPkgsToPkgs :: ATSPackageSet -> PackageSet
atsPkgsToPkgs (ATSPackageSet deps) = PackageSet $ foldr (.) id inserts mempty
    where inserts = insert <$> deps
          insert dep = M.insertWith
            (\_ -> S.insert (libVersion dep))
            (TL.unpack $ libName dep)
            (S.singleton (libVersion dep))

lookupVersions :: ATSPackageSet -> (String, Version) -> ATSDependency
lookupVersions (ATSPackageSet deps) (name, v) = head (filter f deps)
    where f = (&&) <$> matchName <*> matchVersion
          libName' = TL.unpack . libName
          matchName = (== name) . libName'
          matchVersion = (== v) . libVersion
