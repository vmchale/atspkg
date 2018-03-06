{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Development.Shake.ATS.Type ( ForeignCabal (..)
                                  , Version (..)
                                  , ATSTarget (..)
                                  , ArtifactType (..)
                                  , ATSToolConfig (..)
                                  , ATSGen (..)
                                  -- * Lenses
                                  , atsTarget
                                  , hasPretty
                                  , cFlags
                                  , otherDeps
                                  , toolConfig
                                  , cc
                                  , linkStatic
                                  , src
                                  , gc
                                  , binTarget
                                  , genTargets
                                  , hsLibs
                                  , libs
                                  , tgtType
                                  , compilerVer
                                  , libVersion
                                  , linkTargets
                                  ) where

import           Control.Lens
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
data ATSToolConfig = ATSToolConfig { _libVersion  :: Version -- ^ Standard library version to be used.
                                   , _compilerVer :: Version -- ^ Version of @patscc@ to be used.
                                   , _hasPretty   :: Bool -- ^ Whether to display errors via @pats-filter@
                                   , _cc          :: CCompiler -- ^ C compiler to be used
                                   , _linkStatic  :: Bool -- ^ Force static linking
                                   } deriving (Generic, Binary)

data ATSGen = ATSGen { hsFile     :: FilePath -- ^ Haskell file containing types
                     , _atsTarget :: FilePath -- ^ ATS file to be generated
                     , cpphs      :: Bool -- ^ Whether to use the C preprocessor on the Haskell code
                     } deriving (Generic, Binary)

-- | Type for binary and library builds with ATS.
data ATSTarget = ATSTarget { _cFlags      :: [String] -- ^ Flags to be passed to the C compiler
                           , _toolConfig  :: ATSToolConfig -- ^ Configuration options for @patsopt@
                           , _gc          :: Bool -- ^ Whether to build with the garbage collection enabled
                           , _libs        :: [String] -- ^ Libraries against which to link
                           , _src         :: [FilePath] -- ^ ATS source files. If building an executable, at most one may contain @main0@.
                           , _hsLibs      :: [ForeignCabal] -- ^ Cabal-based Haskell libraries.
                           , _genTargets  :: [ATSGen] -- ^ Files to be run through @hs2ats@.
                           , _linkTargets :: [(FilePath, FilePath)] -- ^ Targets for @_link.hats@ generation.
                           , _binTarget   :: FilePath -- ^ Target
                           , _otherDeps   :: [FilePath] -- ^ Other files to track.
                           , _tgtType     :: ArtifactType -- ^ Build type
                           } deriving (Generic, Binary)

-- | Data type containing information about Haskell components of a build. Any
-- functions exposed in the object file will be callable in C or ATS code.
data ForeignCabal = ForeignCabal { projectFile :: Maybe TL.Text -- ^ @cabal.project@ file to track
                                 , cabalFile   :: TL.Text -- ^ @.cabal@ file associated with the library
                                 , objectFile  :: TL.Text -- ^ Object file to be generated
                                 } deriving (Eq, Show, Generic, Binary, Hashable)

makeLenses ''ATSGen
makeLenses ''ATSTarget
makeLenses ''ATSToolConfig
