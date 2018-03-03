{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | This module provides functions for easy C builds of binaries, static
-- libraries, and dynamic libraries.
module Development.Shake.C ( -- * Types
                             CConfig (..)
                           , CCompiler (GCC, Clang, GHC, Other, GCCStd, GHCStd, CompCert)
                           -- * Rules
                           , staticLibR
                           , sharedLibR
                           , objectFileR
                           , dynLibR
                           , cBin
                           , cToLib
                           -- * Actions
                           , ccAction
                           , staticLibA
                           , sharedLibA
                           -- * Helper functions
                           , cconfigToArgs
                           , ccToString
                           , ccFromString
                           , host
                           ) where

import           Control.Monad
import           Data.List                  (isPrefixOf, isSuffixOf)
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Development.Shake
import           Development.Shake.FilePath
import           System.Info

mkQualified :: Monoid a => Maybe a -> Maybe a -> a -> a
mkQualified pre suff = h [f suff, g pre]
    where g = maybe id mappend
          f = maybe id (flip mappend)
          h = foldr fmap id

host :: String
host = arch ++ withManufacturer os
    where withManufacturer "darwin" = "-apple-" ++ os
          withManufacturer _        = "-unknown-" ++ os

pattern GCCStd :: CCompiler
pattern GCCStd = GCC Nothing

pattern GHCStd :: CCompiler
pattern GHCStd = GHC Nothing Nothing

ccToString :: CCompiler -> String
ccToString Clang          = "clang"
ccToString (Other s)      = s
ccToString (GCC pre)      = mkQualified pre Nothing "gcc"
ccToString (GHC pre suff) = mkQualified pre suff "ghc"
ccToString CompCert       = "ccomp"

arToString :: CCompiler -> String
arToString (GCC pre)   = mkQualified pre Nothing "ar"
arToString (GHC pre _) = mkQualified pre Nothing "ar"
arToString _           = "ar"

ccFromString :: String -> CCompiler
ccFromString "gcc" = GCC Nothing
ccFromString "clang" = Clang
ccFromString "ghc" = GHC Nothing Nothing
ccFromString s
    | "gcc" `isSuffixOf` s = GCC (Just (reverse . drop 3 . reverse $ s))
    | "ghc" `isSuffixOf` s = GHC (Just (reverse . drop 3 . reverse $ s)) Nothing
    | "ghc" `isPrefixOf` s = GHC Nothing (Just (drop 3 s))
ccFromString _ = GCC Nothing

data CCompiler = GCC { _prefix :: Maybe String -- ^ Usually the target triple
                     }
               | Clang
               | GHC { _prefix  :: Maybe String -- ^ The target triple
                     , _postfix :: Maybe String -- ^ The compiler version
                     }
               | CompCert
               | Other String
               deriving (Eq)

mapFlags :: String -> ([String] -> [String])
mapFlags s = fmap (s <>)

data CConfig = CConfig { includes   :: [String] -- ^ Directories to be included.
                       , libraries  :: [String] -- ^ Libraries against which to link.
                       , libDirs    :: [String] -- ^ Directories to find libraries.
                       , extras     :: [String] -- ^ Extra flags to be passed to the compiler
                       , staticLink :: Bool -- ^ Whether to link against static versions of libraries
                       }

-- | Rules for making a static library from C source files
cToLib :: CCompiler
       -> [FilePath] -- ^ C source files
       -> FilePattern -- ^ Static libary output
       -> CConfig
       -> Rules ()
cToLib cc sources lib cfg =
    mconcat [ foldr (>>) (pure ()) objRules
            , staticLibR cc (g sources) lib cfg
            ]
    where objRules = objectFileR cc cfg <$> g sources <*> pure lib
          g = fmap (-<.> "o")

cBin :: CCompiler
     -> [FilePath] -- ^ C source files
     -> FilePattern -- ^ Binary file output
     -> CConfig
     -> Rules ()
cBin cc sources bin cfg = bin %> \out -> ccAction cc sources out cfg

ccAction :: CmdResult r
         => CCompiler
         -> [FilePath] -- ^ Source files
         -> FilePath -- ^ Binary file output
         -> CConfig
         -> Action r
ccAction cc sources out cfg =
    need sources >>
    (command [EchoStderr False] (ccToString cc) . (("-o" : out : sources) <>) . cconfigToArgs) cfg

cconfigToArgs :: CConfig -> [String]
cconfigToArgs (CConfig is ls ds es sl) = join [ mapFlags "-I" is, mapFlags "-l" (g sl <$> ls), mapFlags "-L" ds, es ]
    where g :: Bool -> (String -> String)
          g False = id
          g True  = (":lib" <>) . (<> ".a")

dynLibR :: CCompiler
        -> [FilePath]
        -> FilePattern
        -> CConfig
        -> Rules ()
dynLibR cc objFiles shLib cfg =
    shLib %> \out ->
        need objFiles >>
        command [EchoStderr False] (ccToString cc) ("-shared" : "-o" : out : objFiles <> cconfigToArgs cfg)

objectFileR :: CCompiler
            -> CConfig
            -> FilePath -- ^ C source file
            -> FilePattern -- ^ Object file output
            -> Rules ()
objectFileR cc cfg srcFile objFile =
    objFile %> \out ->
        need [srcFile] >>
        command [EchoStderr False] (ccToString cc) (srcFile : "-c" : "-fPIC" : "-o" : out : cconfigToArgs cfg)

sharedLibA :: CmdResult r
           => CCompiler
           -> [FilePath] -- ^ Object files to be linked
           -> FilePattern -- ^ File pattern for shared library outputs
           -> CConfig
           -> Action r
sharedLibA cc objFiles shrLib _ =
    need objFiles >>
    command mempty (ccToString cc) ("-shared" : "-o" : shrLib : objFiles)

staticLibA :: CmdResult r
           => CCompiler
           -> [FilePath] -- ^ Object files to be linked
           -> FilePattern -- ^ File pattern for static library outputs
           -> CConfig
           -> Action r
staticLibA ar objFiles stalib _ =
    need objFiles >>
    command mempty (arToString ar) ("rcs" : stalib : objFiles)

sharedLibR :: CCompiler
           -> [FilePath] -- ^ Object files to be linked
           -> FilePattern -- ^ File pattern for shared library outputs
           -> CConfig
           -> Rules ()
sharedLibR cc objFiles shrLib cfg =
    shrLib %> \out -> sharedLibA cc objFiles out cfg

staticLibR :: CCompiler -- ^ ar binary
           -> [FilePath] -- ^ Object files to be linked
           -> FilePattern -- ^ File pattern for static library outputs
           -> CConfig
           -> Rules ()
staticLibR ar objFiles stalib cfg =
    stalib %> \out -> staticLibA ar objFiles out cfg
