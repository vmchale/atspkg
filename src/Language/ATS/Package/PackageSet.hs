{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Language.ATS.Package.PackageSet ( ATSPackageSet (..)
                                       , setBuildPlan
                                       , mkBuildPlan
                                       ) where

import           Control.Arrow
import           Control.Monad
import           Data.Binary                (decode, encode)
import           Data.Bool                  (bool)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Dependency
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text.Lazy             as TL
import           Dhall                      hiding (bool)
import           Language.ATS.Package.Error
import           Language.ATS.Package.Type
import           System.Directory           (createDirectoryIfMissing, doesFileExist)

-- TODO string instance? string :: Type String

newtype ATSPackageSet = ATSPackageSet [ ATSDependency ]
    deriving (Interpret, Show)

setBuildPlan :: FilePath -- ^ Filepath for cache inside @.atspkg@
             -> [String] -- ^ Libraries we want
             -> IO [[ATSDependency]]
setBuildPlan p deps = do
    b <- doesFileExist depCache
    bool setBuildPlan' (decode <$> BSL.readFile depCache) b

    where depCache = ".atspkg/buildplan-" ++ p
          setBuildPlan' = do
            putStrLn "Resolving dependencies..."
            pkgSet <- input auto "https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/pkg-set.dhall"
            case mkBuildPlan pkgSet deps of
                Just x  -> createDirectoryIfMissing True ".atspkg" >> BSL.writeFile depCache (encode x) >> pure x
                Nothing -> resolutionFailed

mkBuildPlan :: ATSPackageSet -> [String] -> Maybe [[ATSDependency]]
mkBuildPlan aps@(ATSPackageSet ps) = finalize . resolve . fmap asDep <=< stringBuildPlan
    where finalize = fmap (fmap (fmap (lookupVersions aps)))
          resolve = resolveDependencies (atsPkgsToPkgs aps)
          stringBuildPlan names = sequence [ lookup x libs | x <- names ]
              where libs = (TL.unpack . libName &&& id) <$> ps

asDep :: ATSDependency -> Dependency
asDep ATSDependency{..} = Dependency (TL.unpack libName) mempty (TL.unpack <$> libDeps) libVersion

atsPkgsToPkgs :: ATSPackageSet -> PackageSet Dependency
atsPkgsToPkgs (ATSPackageSet deps) = PackageSet $ foldr (.) id inserts mempty
    where inserts = insert <$> deps
          insert dep = M.insertWith
            (\_ -> S.insert (asDep dep))
            (TL.unpack $ libName dep)
            (S.singleton (asDep dep))

lookupVersions :: ATSPackageSet -> Dependency -> ATSDependency
lookupVersions (ATSPackageSet deps) (Dependency name _ _ v) = head (filter f deps)
    where f = (&&) <$> matchName <*> matchVersion
          libName' = TL.unpack . libName
          matchName = (== name) . libName'
          matchVersion = (== v) . libVersion
