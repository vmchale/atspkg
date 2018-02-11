{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}


-- gcc -c -o lib1.o lib1.c
-- gcc ats-src/libnumbertheory.o -shared -o ats-src/libnumbertheory.so
-- gcc number-theory-ffi_dats.c -c -fPIC -o ats-src/libnumbertheory.o -IATS2-Postiats-include-0.3.8/ -IATS2-Postiats-include-0.3.8/ccomp/runtime/

module Development.Shake.ATS.Type ( ForeignCabal (..)
                                  , Version (..)
                                  , BinaryTarget (..)
                                  , ArtifactType (..)
                                  , ATSToolConfig (..)
                                  , CCompiler (GCC, Clang, GHC, Other, GCCStd, GHCStd)
                                  ) where

import           Data.Binary     (Binary (..))
import           Data.Dependency (Version (..))
import qualified Data.Text.Lazy  as TL
import           GHC.Generics    (Generic)

pattern GCCStd :: CCompiler
pattern GCCStd = GCC Nothing Nothing

pattern GHCStd :: CCompiler
pattern GHCStd = GHC Nothing Nothing

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

data ArtifactType = StaticLibrary
                  | DynamicLibrary
                  | Binary Bool -- ^ Whether binary build should create a static binary
                  deriving (Generic, Binary)

data CCompiler = GCC { _prefix :: Maybe String, _suffix :: Maybe String }
               | Clang
               | Other String
               | GHC { _prefix :: Maybe String, _suffix :: Maybe String }
               deriving (Eq, Generic, Binary)

-- | Information about where to find @patscc@ and @patsopt@.
data ATSToolConfig = ATSToolConfig { libVersion  :: Version
                                   , compilerVer :: Version
                                   , hasPretty   :: Bool -- ^ Whether to display errors via @pats-filter@
                                   , cc          :: CCompiler -- ^ C compiler to be used
                                   } deriving (Generic, Binary)

-- | Type for binary and library builds with ATS.
data BinaryTarget = BinaryTarget { cFlags     :: [String] -- ^ Flags to be passed to the C compiler
                                 , toolConfig :: ATSToolConfig
                                 , gc         :: Bool -- ^ Whether to configure build for use with the garbage collector.
                                 , libs       :: [String] -- ^ Libraries against which to link
                                 , src        :: String -- ^ Source file for binary.
                                 , hsLibs     :: [ForeignCabal] -- ^ Cabal-based Haskell libraries
                                 , genTargets :: [(String, String, Bool)] -- ^ Files to be run through @hs2ats@.
                                 , binTarget  :: String -- ^ Binary target
                                 , cDeps      :: [String] -- ^ C files necessary to compile the target
                                 , otherDeps  :: [String] -- ^ Other files necessary to compile target
                                 , tgtType    :: ArtifactType -- ^ Build type
                                 } deriving (Generic, Binary)

-- | Data type containing information about Haskell components of a build.
data ForeignCabal = ForeignCabal { projectFile :: Maybe TL.Text -- ^ @cabal.project@ file to track
                                 , cabalFile   :: TL.Text -- ^ @.cabal@ file associated with the library
                                 , objectFile  :: TL.Text -- ^ Object file to be generated
                                 } deriving (Eq, Show, Generic, Binary)
