{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Development.Shake.ATS.Type ( ForeignCabal (..)
                                  , Version (..)
                                  , ATSTarget (..)
                                  , ArtifactType (..)
                                  , ATSToolConfig (..)
                                  ) where

import           Data.Binary         (Binary (..))
import           Data.Dependency     (Version (..))
import           Data.Hashable       (Hashable)
import qualified Data.Text.Lazy      as TL
import           Development.Shake.C
import           GHC.Generics        (Generic)

-- We should have four build types:
--
-- 1. Static library
--
-- 2. Dynamic library
--
-- 3. Binary
--
-- 4. C sources
--
-- AND variations on these where a Haskell library is to be built
-- Or a C library.
--
-- Also they should account for whatever `atspkg` installs.

-- Package management idea:
--
-- * One big index
--
-- * Versions resolved from that
--
-- * Then we temporarily set the dependencies
--
-- * Ideally something with C dependencies included.
--
-- * Also binary caches are good.

deriving instance Generic CCompiler
deriving instance Binary CCompiler

data ArtifactType = StaticLibrary
                  | Executable
                  | SharedLibrary
                  deriving (Generic, Binary)

-- | Information about where to find @patscc@ and @patsopt@.
data ATSToolConfig = ATSToolConfig { libVersion  :: Version
                                   , compilerVer :: Version -- ^ Version of @patscc@ to be used.
                                   , hasPretty   :: Bool -- ^ Whether to display errors via @pats-filter@
                                   , cc          :: CCompiler -- ^ C compiler to be used
                                   , linkStatic  :: Bool -- ^ Force static linking
                                   } deriving (Generic, Binary)

-- | Type for binary and library builds with ATS.
data ATSTarget = ATSTarget { cFlags      :: [String] -- ^ Flags to be passed to the C compiler
                           , toolConfig  :: ATSToolConfig
                           , gc          :: Bool -- ^ Whether to configure build for use with the garbage collector.
                           , libs        :: [String] -- ^ Libraries against which to link
                           , src         :: [String] -- ^ ATS source files. If building an executable, at most one may contain @main0@.
                           , hsLibs      :: [ForeignCabal] -- ^ Cabal-based Haskell libraries
                           , genTargets  :: [(String, String, Bool)] -- ^ Files to be run through @hs2ats@.
                           , linkTargets :: [(String, String)] -- ^ Targets for @_link.hats@ generation.
                           , binTarget   :: String -- ^ Binary target
                           , otherDeps   :: [String] -- ^ Other files necessary to compile target
                           , tgtType     :: ArtifactType -- ^ Build type
                           } deriving (Generic, Binary)

-- | Data type containing information about Haskell components of a build.
data ForeignCabal = ForeignCabal { projectFile :: Maybe TL.Text -- ^ @cabal.project@ file to track
                                 , cabalFile   :: TL.Text -- ^ @.cabal@ file associated with the library
                                 , objectFile  :: TL.Text -- ^ Object file to be generated
                                 } deriving (Eq, Show, Generic, Binary, Hashable)
