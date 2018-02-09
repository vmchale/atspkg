{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.ATS.Package.Type ( -- * Types
                                   Pkg (..)
                                 , ATSDependency (..)
                                 , Bin (..)
                                 , Version (..)
                                 , ATSConstraint (..)
                                 , TargetPair (..)
                                 , CCompiler (..)
                                 -- * Lenses
                                 , dirLens
                                 ) where

import           Control.Lens
import           Data.Binary           (Binary (..))
import           Data.Dependency
import           Development.Shake.ATS
import           Dhall

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
                                   -- , libDepends :: [Text] -- ^ Strings containing dependencies
                                   }
                deriving (Eq, Show, Generic, Interpret, Binary)

makeLensesFor [("dir", "dirLens")] ''ATSDependency

-- | This is just a tuple, except I can figure out how to use it with Dhall.
data TargetPair = TargetPair { hs  :: Text
                             , ats :: Text
                             } deriving (Eq, Show, Generic, Interpret, Binary)

deriving instance Interpret ForeignCabal

data Bin = Bin { src      :: Text -- ^ Source file (should end with @.dats@)
               , target   :: Text -- ^ Binary to be built
               , libs     :: [Text] -- ^ Libraries to link against (e.g. @[ "pthread" ]@)
               , hsDeps   :: [ForeignCabal] -- ^ Haskell @.cabal@ files associated with the final library we want to make
               , hs2ats   :: [TargetPair] -- ^ List of sources and targets for @hs2ats@
               , gcBin    :: Bool -- ^ Whether to use the garbage collector
               , cSources :: [Text] -- ^ C source files the build depends on
               }
         deriving (Show, Eq, Generic, Interpret, Binary)

-- TODO make binaries optional
-- | Data type associated with @atspkg.dhall@ file.
data Pkg = Pkg { bin          :: [Bin] -- ^ List of binaries to be built
               , test         :: [Bin] -- ^ List of test suites
               , man          :: Maybe Text -- ^ Optional (markdown) manpages to be converted using @pandoc@.
               , version      :: Version -- ^ Library version
               , compiler     :: Version -- ^ Compiler version
               , dependencies :: [ATSDependency] -- ^ List of dependencies
               , clib         :: [ATSDependency] -- ^ List of C dependencies
               , ccompiler    :: Text -- ^ The C compiler we should use
               , cflags       :: [Text] -- ^ List of flags to pass to the C compiler
               , atsSource    :: [Text] -- ^ Directory containing ATS source to be compile to C.
               , cDir         :: Text -- ^ Directory for generated C.
               }
         deriving (Show, Eq, Generic, Interpret, Binary)
