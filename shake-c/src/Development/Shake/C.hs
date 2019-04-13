{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeFamilies       #-}

-- | This module provides functions for easy C builds of binaries, static
-- libraries, and dynamic libraries.
module Development.Shake.C ( -- * Types
                             CConfig (..)
                           , CCompiler (..)
                           -- * Rules
                           , staticLibR
                           , sharedLibR
                           , objectFileR
                           , dynLibR
                           , cBin
                           , cToLib
                           , preprocessA
                           , preprocessR
                           -- * Oracles
                           , idOracle
                           -- * Actions
                           , pkgConfig
                           , binaryA
                           , staticLibA
                           , sharedLibA
                           , stripA
                           -- * Reëxports from "Language.C.Dependency"
                           , getCDepends
                           , getAll
                           -- * Helper functions
                           , cconfigToArgs
                           , ccToString
                           , ccFromString
                           , host
                           , isCross
                           ) where

import           Control.Composition
import           Control.Monad
import           Data.List                  (isPrefixOf, isSuffixOf)
import           Development.Shake          hiding ((*>))
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Language.C.Dependency
import           System.Info

-- | Given a package name or path to a @.pc@ file, output flags for C compiler.
pkgConfig :: String -> Action [String]
pkgConfig pkg = do
    (Stdout o) <- command [] "pkg-config" ["--cflags", pkg]
    (Stdout o') <- command [] "pkg-config" ["--libs", pkg]
    pure (words o ++ words o')

mkQualified :: Monoid a => Maybe a -> Maybe a -> a -> a
mkQualified pre suff = h [f suff, g pre]
    where g = maybe id mappend
          f = maybe id (flip mappend)
          h = thread

-- | The target triple of the host machine.
host :: String
host = arch ++ withManufacturer os
    where withManufacturer "darwin" = "-apple-" ++ os
          withManufacturer _        = "-unknown-" ++ os

-- | Get the executable name for a 'CCompiler'
ccToString :: CCompiler -> String
ccToString ICC            = "icc"
ccToString Clang          = "clang"
ccToString (Other s)      = s
ccToString (GCC pre suff) = mkQualified pre suff "gcc"
ccToString (GHC pre suff) = mkQualified pre suff "ghc"
ccToString CompCert       = "ccomp"

stripToString :: CCompiler -> String
stripToString (GCC pre _) = mkQualified pre Nothing "strip"
stripToString (GHC pre _) = mkQualified pre Nothing "strip"
stripToString _           = "strip"

arToString :: CCompiler -> String
arToString (GCC pre _) = mkQualified pre Nothing "ar"
arToString (GHC pre _) = mkQualified pre Nothing "ar"
arToString _           = "ar"

isCross :: CCompiler -> Bool
isCross (GCC Just{} _) = True
isCross (GHC Just{} _) = True
isCross _              = False

-- | Attempt to parse a string as a 'CCompiler', defaulting to @cc@ if parsing
-- fails.
ccFromString :: String -> CCompiler
ccFromString "icc" = ICC
ccFromString "gcc" = GCC Nothing Nothing
ccFromString "ccomp" = CompCert
ccFromString "clang" = Clang
ccFromString "ghc" = GHC Nothing Nothing
ccFromString s
    | "gcc" `isSuffixOf` s = GCC (Just (reverse . drop 3 . reverse $ s)) Nothing
    | "ghc" `isSuffixOf` s = GHC (Just (reverse . drop 3 . reverse $ s)) Nothing
    | "ghc" `isPrefixOf` s = GHC Nothing (Just (drop 3 s))
    | "gcc" `isPrefixOf` s = GCC Nothing (Just (drop 3 s))
ccFromString _ = Other "cc"

-- ALSO consider using Haskell -> C -> ICC ??
-- TODO TCC
-- | A data type representing the C compiler to be used.
data CCompiler = GCC { _prefix  :: Maybe String -- ^ Usually the target triple
                     , _postfix :: Maybe String -- ^ The compiler version
                     }
               | Clang
               | GHC { _prefix  :: Maybe String -- ^ The target triple
                     , _postfix :: Maybe String -- ^ The compiler version
                     }
               | CompCert
               | ICC
               | Other String
               deriving (Generic, Binary, Show, Typeable, Eq, Hashable, NFData)

mapFlags :: String -> ([String] -> [String])
mapFlags s = fmap (s ++)

data CConfig = CConfig { includes   :: [String] -- ^ Directories to be included.
                       , libraries  :: [String] -- ^ Libraries against which to link.
                       , libDirs    :: [String] -- ^ Directories to find libraries.
                       , extras     :: [String] -- ^ Extra flags to be passed to the compiler
                       , staticLink :: Bool -- ^ Whether to link against static versions of libraries
                       }
             deriving (Generic, Binary, Show, Typeable, Eq, Hashable, NFData)

type instance RuleResult CCompiler = CCompiler
type instance RuleResult CConfig = CConfig

-- | Use this for tracking e.g. 'CCompiler' or 'CConfig'
--
-- @since 0.4.1.0
idOracle :: (RuleResult q ~ a, q ~ a, ShakeValue q) => Rules (q -> Action a)
idOracle = addOracle pure

-- | Rules for making a static library from C source files. Unlike 'staticLibR',
-- this also creates rules for creating object files.
cToLib :: CCompiler
       -> [FilePath] -- ^ C source files
       -> FilePattern -- ^ Static libary output
       -> CConfig
       -> Rules ()
cToLib cc sources lib cfg =
    sequence_ ( staticLibR cc (g sources) lib cfg : objRules)
    where objRules = objectFileR cc cfg <$> g sources <*> pure lib
          g = fmap (-<.> "o")

-- | Rules for preprocessing a C source file.
--
-- @since 0.4.3.0
preprocessR :: CCompiler
            -> FilePath -- ^ C source file
            -> FilePattern -- ^ Preprocessed file output
            -> CConfig
            -> Rules ()
preprocessR cc source proc cfg = proc %> \out -> preprocessA cc source out cfg

-- | Rules for generating a binary from C source files. Can have at most have
-- one @main@ function.
cBin :: CCompiler
     -> [FilePath] -- ^ C source files
     -> FilePattern -- ^ Binary file output
     -> CConfig
     -> Rules ()
cBin cc sources bin cfg = bin %> \out -> binaryA cc sources out cfg
-- TODO depend on config!!
-- TODO depend on the source files transitively (optionally)

stripA :: CmdResult r
       => FilePath -- ^ Build product to be stripped
       -> CCompiler -- ^ C compiler
       -> Action r
stripA out cc = command mempty (stripToString cc) [out]

-- | @since 0.4.3.0
preprocessA :: CmdResult r
            => CCompiler
            -> FilePath -- ^ Source file
            -> FilePath -- ^ Preprocessed output
            -> CConfig
            -> Action r
preprocessA cc source out cfg =
    need [source] *>
    (command [EchoStderr False] (ccToString cc) . (("-E" : "-o" : out : [source]) ++) . cconfigToArgs) cfg

-- | This action builds an executable.
binaryA :: CmdResult r
        => CCompiler
        -> [FilePath] -- ^ Source files
        -> FilePath -- ^ Executable output
        -> CConfig
        -> Action r
binaryA cc sources out cfg =
    need sources *>
    (command [EchoStderr False] (ccToString cc) . (("-o" : out : sources) ++) . cconfigToArgs) cfg

-- | Generate compiler flags for a given configuration.
cconfigToArgs :: CConfig -> [String]
cconfigToArgs (CConfig is ls ds es sl) = join [ mapFlags "-I" is, mapFlags "-l" (g sl <$> ls), mapFlags "-L" ds, es ]
    where g :: Bool -> (String -> String)
          g False = id
          g True  = (":lib" ++) . (++ ".a")

-- | These rules build a dynamic library (@.so@ on Linux).
dynLibR :: CCompiler
        -> [FilePath] -- ^ C source files
        -> FilePattern -- ^ Shared object file to be generated.
        -> CConfig
        -> Rules ()
dynLibR cc objFiles shLib cfg =
    shLib %> \out -> do
        need objFiles
        command [EchoStderr False] (ccToString cc) ("-shared" : "-o" : out : objFiles ++ cconfigToArgs cfg)

-- | These rules build an object file from a C source file.
objectFileR :: CCompiler
            -> CConfig
            -> FilePath -- ^ C source file
            -> FilePattern -- ^ Object file output
            -> Rules ()
objectFileR cc cfg srcFile objFile =
    objFile %> \out -> do
        need [srcFile]
        command [EchoStderr False] (ccToString cc) (srcFile : "-c" : "-fPIC" : "-o" : out : cconfigToArgs cfg)

sharedLibA :: CmdResult r
           => CCompiler
           -> [FilePath] -- ^ Object files to be linked
           -> FilePattern -- ^ File pattern for shared library outputs
           -> CConfig
           -> Action r
sharedLibA cc objFiles shrLib _ =
    need objFiles *>
    command mempty (ccToString cc) ("-shared" : "-o" : shrLib : objFiles)

staticLibA :: CmdResult r
           => CCompiler
           -> [FilePath] -- ^ Object files to be linked
           -> FilePattern -- ^ File pattern for static library outputs
           -> CConfig
           -> Action r
staticLibA ar objFiles stalib _ =
    need objFiles *>
    command mempty (arToString ar) ("rcs" : stalib : objFiles)

sharedLibR :: CCompiler
           -> [FilePath] -- ^ Object files to be linked
           -> FilePattern -- ^ File pattern for shared library outputs
           -> CConfig
           -> Rules ()
sharedLibR cc objFiles shrLib cfg =
    shrLib %> \out ->
        sharedLibA cc objFiles out cfg

staticLibR :: CCompiler
           -> [FilePath] -- ^ Object files to be linked
           -> FilePattern -- ^ File pattern for static library outputs
           -> CConfig
           -> Rules ()
staticLibR ar objFiles stalib cfg =
    stalib %> \out ->
        staticLibA ar objFiles out cfg
