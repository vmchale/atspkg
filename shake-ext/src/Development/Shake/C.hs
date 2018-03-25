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
                           , binaryA
                           , staticLibA
                           , sharedLibA
                           -- * Helper functions
                           , cconfigToArgs
                           , ccToString
                           , ccFromString
                           , getCDepends
                           , host
                           ) where

import           Control.Monad
import           Data.List                  (isPrefixOf, isSuffixOf)
import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory           (removeFile)
import           System.Info

-- | Given C source code, return a list of included files. This makes a call to
-- either @clang@ or @gcc@, so it should be used sparingly.
getCDepends :: CCompiler -- ^ Should be either @gcc@ or @clang@.
            -> String -- ^ C source code
            -> Action [FilePath]
getCDepends cc' src = do
    liftIO $ writeFile "shake.c" src
    (Stdout o) <- command [] (ccToString cc') ["-MM", "shake.c"]
    liftIO $ removeFile "shake.c"
    pure (drop 1 . filter (/= "/") $ words o)

mkQualified :: Monoid a => Maybe a -> Maybe a -> a -> a
mkQualified pre suff = h [f suff, g pre]
    where g = maybe id mappend
          f = maybe id (flip mappend)
          h = foldr fmap id

-- | The target triple of the host machine.
host :: String
host = arch ++ withManufacturer os
    where withManufacturer "darwin" = "-apple-" ++ os
          withManufacturer _        = "-unknown-" ++ os

-- | Default @gcc@ available
pattern GCCStd :: CCompiler
pattern GCCStd = GCC Nothing

-- | Default @ghc@ available
pattern GHCStd :: CCompiler
pattern GHCStd = GHC Nothing Nothing

-- | Get the executable name associated with a 'CCompiler'
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

-- | Attempt to parse a string as a 'CCompiler', defaulting to @cc@ if parsing
-- fails.
ccFromString :: String -> CCompiler
ccFromString "gcc" = GCC Nothing
ccFromString "clang" = Clang
ccFromString "ghc" = GHC Nothing Nothing
ccFromString s
    | "gcc" `isSuffixOf` s = GCC (Just (reverse . drop 3 . reverse $ s))
    | "ghc" `isSuffixOf` s = GHC (Just (reverse . drop 3 . reverse $ s)) Nothing
    | "ghc" `isPrefixOf` s = GHC Nothing (Just (drop 3 s))
ccFromString _ = Other "cc"

-- | A data type representing the C compiler to be used.
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

-- | Rules for making a static library from C source files. Unlike 'staticLibR',
-- this also creates rules for creating object files.
cToLib :: CCompiler
       -> [FilePath] -- ^ C source files
       -> FilePattern -- ^ Static libary output
       -> CConfig
       -> Rules ()
cToLib cc sources lib cfg =
    mconcat [ mconcat objRules
            , staticLibR cc (g sources) lib cfg
            ]
    where objRules = objectFileR cc cfg <$> g sources <*> pure lib
          g = fmap (-<.> "o")

-- | Rules for generating a binary from C source files. At most one can have the
-- @main@ function.
cBin :: CCompiler
     -> [FilePath] -- ^ C source files
     -> FilePattern -- ^ Binary file output
     -> CConfig
     -> Rules ()
cBin cc sources bin cfg = bin %> \out -> binaryA cc sources out cfg

-- | This action builds a binary.
binaryA :: CmdResult r
         => CCompiler
         -> [FilePath] -- ^ Source files
         -> FilePath -- ^ Binary file output
         -> CConfig
         -> Action r
binaryA cc sources out cfg =
    need sources >>
    (command [EchoStderr False] (ccToString cc) . (("-o" : out : sources) <>) . cconfigToArgs) cfg

cconfigToArgs :: CConfig -> [String]
cconfigToArgs (CConfig is ls ds es sl) = join [ mapFlags "-I" is, mapFlags "-l" (g sl <$> ls), mapFlags "-L" ds, es ]
    where g :: Bool -> (String -> String)
          g False = id
          g True  = (":lib" <>) . (<> ".a")

-- | These rules build a dynamic library (@.so@ on Linux).
dynLibR :: CCompiler
        -> [FilePath] -- ^ C source files
        -> FilePattern -- ^ Shared object file to be generated.
        -> CConfig
        -> Rules ()
dynLibR cc objFiles shLib cfg =
    shLib %> \out ->
        need objFiles >>
        command [EchoStderr False] (ccToString cc) ("-shared" : "-o" : out : objFiles <> cconfigToArgs cfg)

-- | These rules build an object file from a C source file.
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

staticLibR :: CCompiler
           -> [FilePath] -- ^ Object files to be linked
           -> FilePattern -- ^ File pattern for static library outputs
           -> CConfig
           -> Rules ()
staticLibR ar objFiles stalib cfg =
    stalib %> \out -> staticLibA ar objFiles out cfg
