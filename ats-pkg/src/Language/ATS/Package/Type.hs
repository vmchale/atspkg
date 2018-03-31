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
                                 , Src (..)
                                 , Version (..)
                                 , ForeignCabal (..)
                                 , ATSConstraint (..)
                                 , TargetPair (..)
                                 , CCompiler (..)
                                 , LibDep
                                 , DepSelector
                                 -- * Lenses
                                 , dirLens
                                 ) where

import           Data.Dependency
import           Data.Hashable         (Hashable)
import           Development.Shake.ATS
import           Quaalude

data ATSConstraint = ATSConstraint { lower :: Maybe Version
                                   , upper :: Maybe Version
                                   }
                deriving (Eq, Show, Generic, Binary, Interpret, Hashable)

deriving newtype instance Inject Version
deriving newtype instance Interpret Version
deriving newtype instance Hashable Version

type LibDep = (Text, ATSConstraint)

-- | You likely want 'libDeps' or 'libBldDeps'
type DepSelector = ATSDependency -> [LibDep]

-- TODO add a field for configure stage??
-- | Type for a dependency
data ATSDependency = ATSDependency { libName     :: Text -- ^ Library name, e.g.
                                   , dir         :: Text -- ^ Directory we should unpack to
                                   , url         :: Text -- ^ Url pointing to tarball
                                   , description :: Maybe Text -- ^ Package description
                                   , libVersion  :: Version
                                   , libDeps     :: [LibDep] -- ^ Dependencies to be unpacked
                                   , libBldDeps  :: [LibDep] -- ^ Dependencies to be built
                                   , libCDeps    :: [LibDep] -- ^ C dependencies to be built
                                   , script      :: [Text] -- ^ Optional build script for C library
                                   }
                   deriving (Eq, Show, Generic, Interpret, Binary, Hashable)

makeLensesFor [("dir", "dirLens")] ''ATSDependency

-- | This is just a tuple, except I can figure out how to use it with Dhall.
data TargetPair = TargetPair { hs    :: Text
                             , ats   :: Text
                             , cpphs :: Bool
                             } deriving (Eq, Show, Generic, Interpret, Binary, Hashable)

deriving instance Interpret ForeignCabal

data Src = Src { atsSrc  :: Text
               , cTarget :: Text
               , atsGen  :: [TargetPair]
               , extras  :: [Text]
               }
         deriving (Show, Eq, Generic, Interpret, Binary, Hashable)

data Bin = Bin { src    :: Text -- ^ Source file (should end with @.dats@)
               , target :: Text -- ^ Binary to be built
               , libs   :: [Text] -- ^ Libraries to link against (e.g. @[ "pthread" ]@)
               , hsDeps :: [ForeignCabal] -- ^ Haskell @.cabal@ files associated with the final library we want to make
               , hs2ats :: [TargetPair] -- ^ List of sources and targets for @hs2ats@
               , gcBin  :: Bool -- ^ Whether to use the garbage collector
               , extras :: [Text] -- ^ Extra source files the build depends on
               }
         deriving (Show, Eq, Generic, Interpret, Binary, Hashable)

data Lib = Lib { name      :: Text -- ^ Name of library being provided
               , src       :: [Text] -- ^ Source files (should end with @.dats@) to be compiled to object files
               , libTarget :: Text
               , libs      :: [Text] -- ^ Libraries to link against (e.g. @[ "pthread" ]@)
               , includes  :: [Text] -- ^ Include files to be installed with the library
               , hsDeps    :: [ForeignCabal] -- ^ Haskell @.cabal@ files associated with object files
               , links     :: [(Text, Text)] -- ^ Generate link files.
               , hs2ats    :: [TargetPair] -- ^ Sources and targets for @hs2ats@
               , extras    :: [Text] -- ^ Other source files the build depends on
               , static    :: Bool -- ^ Whether to make a static library
               }
         deriving (Show, Eq, Generic, Interpret, Binary, Hashable)

-- | Data type associated with @atspkg.dhall@ file.
data Pkg = Pkg { bin          :: [Bin] -- ^ List of binaries to be built
               , test         :: [Bin] -- ^ List of test suites
               , libraries    :: [Lib] -- ^ List of libraries to be built
               , man          :: Maybe Text -- ^ Optional (markdown) manpages to be converted using @pandoc@.
               , completions  :: Maybe Text -- ^ Optional @compleat@ completions to be installed alongside package.
               , version      :: Version -- ^ Library version
               , compiler     :: Version -- ^ Compiler version
               , dependencies :: [LibDep] -- ^ List of library dependencies
               , clib         :: [LibDep] -- ^ List of C library dependencies
               , buildDeps    :: [LibDep] -- ^ List of ATS library dependencies
               , ccompiler    :: Text -- ^ The C compiler we should use
               , cflags       :: [Text] -- ^ List of flags to pass to the C compiler
               , atsSource    :: [Src] -- ^ ATS source to be compile to C.
               , dynLink      :: Bool -- ^ Don't link statically, instead, use libraries installed by @atspkg@.
               }
         deriving (Show, Eq, Generic, Interpret, Binary, Hashable)
