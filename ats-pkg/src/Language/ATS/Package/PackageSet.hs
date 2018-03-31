{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.ATS.Package.PackageSet ( ATSPackageSet (..)
                                       , setBuildPlan
                                       , displayList
                                       ) where

import qualified Data.ByteString.Lazy       as BSL
import           Data.Dependency
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Language.ATS.Package.Error
import           Language.ATS.Package.Type
import           Quaalude

newtype ATSPackageSet = ATSPackageSet { _atsPkgSet :: [ ATSDependency ] }
    deriving (Interpret, Show)

makeLenses ''ATSPackageSet

instance Pretty Version where
    pretty v = text (show v)

instance Pretty ATSDependency where
    pretty (ATSDependency ln _ url md v _ _ _ _) = dullyellow (text (unpack ln)) <#> indent 4 (g md "url:" <+> text (unpack url) <#> "version:" <+> pretty v) <> hardline
        where g (Just d) = ("description:" <+> text (unpack d) <#>)
              g Nothing  = id

instance Pretty ATSPackageSet where
    pretty (ATSPackageSet ds) = mconcat (punctuate hardline (pretty <$> ds))

displayList :: String -> IO ()
displayList = putDoc . pretty <=< listDeps True

listDeps :: Bool -- ^ Whether to sort dependencies
         -> String -- ^ URL of package set
         -> IO ATSPackageSet
listDeps b = fmap s . input auto . pack
    where s = bool id s' b
          s' = over atsPkgSet (sortBy (compare `on` libName))

setBuildPlan :: FilePath -- ^ Filepath for cache inside @.atspkg@
             -> DepSelector
             -> String -- ^ URL of package set to use.
             -> [String] -- ^ Libraries we want
             -> IO [[ATSDependency]]
setBuildPlan p getDeps url deps = do
    b <- doesFileExist depCache
    bool setBuildPlan' (decode <$> BSL.readFile depCache) b

    where depCache = ".atspkg/buildplan-" ++ p
          setBuildPlan' = do
            pkgSet <- listDeps False url
            case mkBuildPlan getDeps pkgSet deps of
                Left x  -> resolutionFailed x
                Right x -> createDirectoryIfMissing True ".atspkg" >>
                           BSL.writeFile depCache (encode x) >>
                           pure x

mkBuildPlan :: DepSelector
            -> ATSPackageSet
            -> [String]
            -> DepM [[ATSDependency]]
mkBuildPlan getDeps aps@(ATSPackageSet ps) = finalize . resolve . fmap (asDep getDeps) <=< stringBuildPlan
    where finalize = fmap (fmap (fmap (lookupVersions aps)))
          resolve = resolveDependencies (atsPkgsToPkgs libDeps aps)
          stringBuildPlan names = sequence [ lookup' x libs | x <- names ]
              where libs = (unpack . libName &&& id) <$> ps
                    lookup' k vs = case lookup k vs of
                        Just x  -> Right x
                        Nothing -> Left (NotPresent k)

canonicalize :: ATSConstraint -> Constraint Version
canonicalize (ATSConstraint (Just l) Nothing)  = GreaterThanEq l
canonicalize (ATSConstraint Nothing (Just u))  = LessThanEq u
canonicalize (ATSConstraint Nothing Nothing)   = None
canonicalize (ATSConstraint (Just l) (Just u)) = Bounded (GreaterThanEq l) (LessThanEq u)

asDep :: DepSelector
      -> ATSDependency
      -> Dependency
asDep getDeps d@ATSDependency{..} = Dependency (unpack libName) (g <$> getDeps d) libVersion
    where g = unpack *** canonicalize

atsPkgsToPkgs :: DepSelector
              -> ATSPackageSet
              -> PackageSet Dependency
atsPkgsToPkgs getDeps (ATSPackageSet deps) = PackageSet $ foldr (.) id inserts mempty
    where inserts = insert <$> deps
          insert dep = M.insertWith
            (\_ -> S.insert (asDep getDeps dep))
            (unpack $ libName dep)
            (S.singleton (asDep getDeps dep))

lookupVersions :: ATSPackageSet -> Dependency -> ATSDependency
lookupVersions (ATSPackageSet deps) (Dependency name _ v) = head (filter f deps)
    where f = (&&) <$> matchName <*> matchVersion
          libName' = unpack . libName
          matchName = (== name) . libName'
          matchVersion = (== v) . libVersion
