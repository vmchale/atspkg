{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.ATS.Package.Type ( -- * Types
                                   Pkg (..)
                                 , ATSDependency (..)
                                 , Bin (..)
                                 , Lib (..)
                                 , Version (..)
                                 , ForeignCabal (..)
                                 , ATSConstraint (..)
                                 , TargetPair (..)
                                 , CCompiler (..)
                                 -- * Lenses
                                 , dirLens
                                 ) where

import           Data.Dependency
import           Development.Shake.ATS
import           Quaalude

data ATSConstraint = ATSConstraint { pkgName :: Text
                                   , lower   :: Maybe Version
                                   , upper   :: Maybe Version
                                   }
                deriving (Eq, Show, Generic, Interpret)

deriving newtype instance Interpret Version

-- TODO make this a map from versions to tarballs etc.
-- | Type for a dependency
data ATSDependency = ATSDependency { libName    :: Text -- ^ Library name, e.g.
                                   , dir        :: Text -- ^ Directory we should unpack to
                                   , url        :: Text -- ^ Url pointing to tarball
                                   , libVersion :: Version
                                   , libDeps    :: [Text] -- ^ Strings containing dependencies
                                   }
                deriving (Eq, Show, Generic, Interpret, Binary)

makeLensesFor [("dir", "dirLens")] ''ATSDependency

-- | This is just a tuple, except I can figure out how to use it with Dhall.
data TargetPair = TargetPair { hs    :: Text
                             , ats   :: Text
                             , cpphs :: Bool
                             } deriving (Eq, Show, Generic, Interpret, Binary)

deriving instance Interpret ForeignCabal


data Bin = Bin { src      :: Text -- ^ Source file (should end with @.dats@)
               , target   :: Text -- ^ Binary to be built
               , libs     :: [Text] -- ^ Libraries to link against (e.g. @[ "pthread" ]@)
               , hsDeps   :: [ForeignCabal] -- ^ Haskell @.cabal@ files associated with the final library we want to make
               , hs2ats   :: [TargetPair] -- ^ List of sources and targets for @hs2ats@
               , gcBin    :: Bool -- ^ Whether to use the garbage collector
               , cSources :: [Text] -- ^ C source files the build depends on
               , extras   :: [Text] -- ^ Extra source files the build depends on
               }
         deriving (Show, Eq, Generic, Interpret, Binary)

data Lib = Lib { name      :: Text -- ^ Name of library being provided
               , src       :: [Text] -- ^ Source files (should end with @.dats@) to be compiled to object files
               , libTarget :: Text
               , libs      :: [Text] -- ^ Libraries to link against (e.g. @[ "pthread" ]@)
               , includes  :: [Text] -- ^ Include files to be installed with the library
               , hsDeps    :: [ForeignCabal] -- ^ Haskell @.cabal@ files associated with object files
               , hs2ats    :: [TargetPair] -- ^ Sources and targets for @hs2ats@
               , cSources  :: [Text] -- ^ C source files the build depends on
               , extras    :: [Text] -- ^ Other source files the build depends on
               }
         deriving (Show, Eq, Generic, Interpret, Binary)

-- TODO make binaries optional
-- | Data type associated with @atspkg.dhall@ file.
data Pkg = Pkg { bin          :: [Bin] -- ^ List of binaries to be built
               , test         :: [Bin] -- ^ List of test suites
               , libraries    :: [Lib] -- ^ List of libraries to be built
               , man          :: Maybe Text -- ^ Optional (markdown) manpages to be converted using @pandoc@.
               , version      :: Version -- ^ Library version
               , compiler     :: Version -- ^ Compiler version
               , dependencies :: [Text] -- ^ List of dependencies
               , clib         :: [Text] -- ^ List of C dependencies
               , buildDeps    :: [Text] -- ^ List of ATS dependencies that should be installed as static libraries
               , ccompiler    :: Text -- ^ The C compiler we should use
               , cflags       :: [Text] -- ^ List of flags to pass to the C compiler
               , atsSource    :: [Text] -- ^ Directory containing ATS source to be compile to C.
               , cDir         :: Text -- ^ Directory for generated C.
               }
         deriving (Show, Eq, Generic, Interpret, Binary)
