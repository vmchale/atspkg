module Language.ATS.Package.Contents ( parsePackageContents
                                     , PackageContents (..)
                                     ) where

data PackageContents = PackageContents { _srcDirs    :: [FilePath]
                                       , _files      :: [FilePath]
                                       , _binTargets :: [(FilePath, FilePath)]
                                       , _libTargets :: [(FilePath, FilePath)] -- for foreign libraries (builds a c dependency)
                                       } deriving (Show, Eq)

parsePackageContents :: IO PackageContents
parsePackageContents = pure $ PackageContents mempty mempty mempty mempty
