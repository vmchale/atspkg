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
                                  , HATSGen (..)
                                  , Solver (..)
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
                                  , patsHome
                                  , patsHomeLocs
                                  , tgtType
                                  , linkTargets
                                  , cpphs
                                  , hsFile
                                  , strip
                                  , solver
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

data Solver = PatsSolve
            | Z3
            | Ignore
            deriving (Generic, Binary)

-- | Information about where to find @patscc@ and @patsopt@.
data ATSToolConfig = ATSToolConfig { _patsHome     :: String -- ^ Value to be used for @PATSHOME@.
                                   , _patsHomeLocs :: String -- ^ Value to be used for @PATSHOMELOCS@.
                                   , _hasPretty    :: Bool -- ^ Whether to display errors via @pats-filter@
                                   , _cc           :: CCompiler -- ^ C compiler to be used
                                   , _linkStatic   :: Bool -- ^ Force static linking
                                   , _solver       :: Solver
                                   -- , _linkATSLib :: Bool -- ^ Whether to link against atslib
                                   } deriving (Generic, Binary)

data HATSGen = HATSGen { satsFile :: FilePath -- ^ @.sats@ file containing type definitions
                       , hatsFile :: FilePath -- ^ @.hats@ file to be generated for library distribution
                       } deriving (Generic, Binary)

data ATSGen = ATSGen { _hsFile    :: FilePath -- ^ Haskell file containing types
                     , _atsTarget :: FilePath -- ^ ATS file to be generated
                     , _cpphs     :: Bool -- ^ Whether to use the C preprocessor on the Haskell code
                     } deriving (Generic, Binary)

-- TODO split off haskell-related types and leave it more general??

-- | Type for binary and library builds with ATS.
data ATSTarget = ATSTarget { _cFlags      :: [String] -- ^ Flags to be passed to the C compiler
                           , _toolConfig  :: ATSToolConfig -- ^ Configuration options for @patsopt@
                           , _gc          :: Bool -- ^ Whether to build with the garbage collection enabled
                           , _libs        :: [String] -- ^ Libraries against which to link
                           , _src         :: [FilePath] -- ^ ATS source files. If building an executable, at most one may contain @main0@.
                           , _hsLibs      :: [ForeignCabal] -- ^ Cabal-based Haskell libraries.
                           , _genTargets  :: [ATSGen] -- ^ Files to be run through @hs2ats@.
                           , _linkTargets :: [HATSGen] -- ^ Targets for @_link.hats@ generation.
                           , _binTarget   :: FilePath -- ^ Target
                           , _otherDeps   :: [FilePath] -- ^ Other files to track.
                           , _tgtType     :: ArtifactType -- ^ Build type
                           , _strip       :: Bool -- ^ Whether to strip generated artifacts
                           } deriving (Generic, Binary)

-- | Data type containing information about Haskell components of a build. Any
-- functions exposed in the object file will be callable in C or ATS code.
data ForeignCabal = ForeignCabal { projectFile :: Maybe TL.Text -- ^ @cabal.project@ file to track
                                 , cabalFile   :: TL.Text -- ^ @.cabal@ file associated with the library
                                 , objectFile  :: TL.Text -- ^ Object file to be generated
                                 } deriving (Eq, Show, Generic, Binary, Hashable)

mconcat <$> traverse makeLenses [''ATSGen, ''ATSTarget, ''ATSToolConfig]
