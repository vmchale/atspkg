{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

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
import           Development.Shake.ATS
import           Language.ATS.Package.Debian
import           Quaalude

data ATSConstraint = ATSConstraint { lower :: Maybe Version
                                   , upper :: Maybe Version
                                   }
                deriving (Eq, Show, Generic, Binary, FromDhall)

deriving newtype instance ToDhall Version

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
                   deriving (Generic, FromDhall, Binary)

dirLens :: Lens' ATSDependency Text
dirLens f s = fmap (\x -> s { dir = x }) (f (dir s))

-- | This is just a tuple, except I can figure out how to use it with Dhall.
data TargetPair = TargetPair { hs    :: Text
                             , ats   :: Text
                             , cpphs :: Bool
                             } deriving (Generic, FromDhall, Binary)

deriving instance FromDhall ForeignCabal

deriving instance FromDhall Solver

data Src = Src { atsSrc  :: Text
               , cTarget :: Text
               , atsGen  :: [TargetPair]
               , extras  :: [Text]
               }
         deriving (Generic, FromDhall, Binary)

data Bin = Bin { src    :: Text -- ^ Source file (should end with @.dats@)
               , target :: Text -- ^ Binary to be built
               , libs   :: [Text] -- ^ Libraries to link against (e.g. @[ "pthread" ]@)
               , hsDeps :: [ForeignCabal] -- ^ Haskell @.cabal@ files associated with the final library we want to make
               , hs2ats :: [TargetPair] -- ^ List of sources and targets for @hs2ats@
               , gcBin  :: Bool -- ^ Whether to use the garbage collector
               , extras :: [Text] -- ^ Extra source files the build depends on
               }
         deriving (Generic, FromDhall, Binary)

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
         deriving (Generic, FromDhall, Binary)

-- | Data type associated with @atspkg.dhall@ file.
data Pkg = Pkg { bin          :: [Bin] -- ^ List of binaries to be built
               , test         :: [Bin] -- ^ List of test suites
               , bench        :: [Bin] -- ^ List of benchmarks
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
               , atsFlags     :: [Text] -- ^ List of flags to pass to @patsopt@.
               , atsSource    :: [Src] -- ^ ATS source to be compile to C.
               , dynLink      :: Bool -- ^ Don't link statically, instead, use libraries installed by @atspkg@.
               , extSolve     :: Solver -- ^ Solver to use.
               , debPkg       :: Maybe Debian -- ^ Optional specificiation as a debian package.
               , atsLib       :: Bool -- ^ Whether to link/build @atslib@.
               }
         deriving (Generic, FromDhall, Binary)
