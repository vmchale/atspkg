{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Language.ATS.Package.PackageSet ( ATSPackageSet (..)
                                       , setBuildPlan
                                       , mkBuildPlan
                                       ) where

import qualified Data.ByteString.Lazy       as BSL
import           Data.Dependency
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Language.ATS.Package.Error
import           Language.ATS.Package.Type
import           Quaalude

-- TODO string instance? string :: Type String

newtype ATSPackageSet = ATSPackageSet [ ATSDependency ]
    deriving (Interpret, Show)

setBuildPlan :: FilePath -- ^ Filepath for cache inside @.atspkg@
             -> String -- ^ URL of package set to use.
             -> [String] -- ^ Libraries we want
             -> IO [[ATSDependency]]
setBuildPlan p url deps = do
    b <- doesFileExist depCache
    bool setBuildPlan' (decode <$> BSL.readFile depCache) b

    where depCache = ".atspkg/buildplan-" ++ p
          setBuildPlan' = do
            pkgSet <- input auto (pack url)
            case mkBuildPlan pkgSet deps of
                Right x -> createDirectoryIfMissing True ".atspkg" >> BSL.writeFile depCache (encode x) >> pure x
                Left x  -> resolutionFailed x

mkBuildPlan :: ATSPackageSet -> [String] -> DepM [[ATSDependency]]
mkBuildPlan aps@(ATSPackageSet ps) = finalize . resolve . fmap asDep <=< stringBuildPlan
    where finalize = fmap (fmap (fmap (lookupVersions aps)))
          resolve = resolveDependencies (atsPkgsToPkgs aps)
          stringBuildPlan names = sequence [ lookup' x libs | x <- names ]
              where libs = (unpack . libName &&& id) <$> ps
                    lookup' k vs = case lookup k vs of
                        Just x  -> Right x
                        Nothing -> Left (NotPresent k)

asDep :: ATSDependency -> Dependency
asDep ATSDependency{..} = Dependency (unpack libName) mempty (unpack <$> libDeps) libVersion

atsPkgsToPkgs :: ATSPackageSet -> PackageSet Dependency
atsPkgsToPkgs (ATSPackageSet deps) = PackageSet $ foldr (.) id inserts mempty
    where inserts = insert <$> deps
          insert dep = M.insertWith
            (\_ -> S.insert (asDep dep))
            (unpack $ libName dep)
            (S.singleton (asDep dep))

lookupVersions :: ATSPackageSet -> Dependency -> ATSDependency
lookupVersions (ATSPackageSet deps) (Dependency name _ _ v) = head (filter f deps)
    where f = (&&) <$> matchName <*> matchVersion
          libName' = unpack . libName
          matchName = (== name) . libName'
          matchVersion = (== v) . libVersion
