{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Development.Shake.ATS ( -- * Shake Rules
                               cgen
                             , cgenPretty
                             , cleanATS
                             , atsBin
                             , atsLex
                             , cabalExport
                             -- * Actions
                             , patsHome
                             -- * Helper functions
                             , getSubdirs
                             , ccToString
                             , ccFromString
                             , ccToDir
                             , compatible
                             , host
                             , patscc
                             , patsopt
                             -- Types
                             , Version (..)
                             , ForeignCabal (..)
                             , BinaryTarget (..)
                             , ATSToolConfig (..)
                             , CCompiler (..)
                             , ArtifactType (..)
                             ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bool                         (bool)
import           Data.Either                       (fromRight)
import           Data.List                         (intercalate)
import           Data.Maybe                        (fromMaybe)
import           Data.Semigroup                    (Semigroup (..))
import qualified Data.Text.Lazy                    as TL
import           Development.Shake                 hiding (doesFileExist)
import           Development.Shake.ATS.Environment
import           Development.Shake.ATS.Rules
import           Development.Shake.ATS.Type
import           Development.Shake.C
import           Development.Shake.FilePath
import           Development.Shake.Version
import           Language.ATS
import           Lens.Micro
import           System.Directory                  (copyFile, createDirectoryIfMissing, doesFileExist)
import           System.Exit                       (ExitCode (ExitSuccess))

-- | Whether generated libraries are to be considered compatible.
compatible :: CCompiler -> CCompiler -> Bool
compatible GCCStd Clang = True
compatible Clang GCCStd = True
compatible x y          = x == y

-- | Run @patsopt@ given information about various things
atsCommand :: CmdResult r => ATSToolConfig
                          -> String -- ^ Source file
                          -> String -- ^ C code to be generated
                          -> Action r
atsCommand tc sourceFile out = do
    path <- fromMaybe "" <$> getEnv "PATH"
    home' <- home tc
    let env = patsEnv home' path
    patsc <- patsopt tc
    command env patsc ["--output", out, "-dd", sourceFile, "-cc"]

gcFlag :: Bool -> String
gcFlag False = "-DATS_MEMALLOC_LIBC"
gcFlag True  = "-DATS_MEMALLOC_GCBDW"

-- Copy source files to the appropriate place. This is necessary because
-- @#include@s in ATS are weird.
copySources :: ATSToolConfig -> [FilePath] -> Action ()
copySources (ATSToolConfig v v' _ _) sources =
    forM_ sources $ \dep -> do
        h <- patsHome v'
        let home' = h ++ "lib/ats2-postiats-" ++ show v
        liftIO $ createDirectoryIfMissing True (home' ++ "/" ++ takeDirectory dep)
        liftIO $ copyFile dep (home' ++ "/" ++ dep)

-- This is the @$PATSHOMELOCS@ variable to be passed to the shell.
patsHomeLocs :: Int -> String
patsHomeLocs n = intercalate ":" $ (<> ".atspkg/contrib") . ("./" <>) <$> g
    where g = [ join $ replicate i "../" | i <- [1..n] ]

makeCFlags :: [String] -- ^ Inputs
           -> [ForeignCabal] -- ^ Haskell libraries
           -> String -- ^ GHC version
           -> Bool -- ^ Whether to use the Garbage collector
           -> [String]
makeCFlags ss fc ghcV' b = gcFlag' : (hsExtra <> ss) where
    gcFlag' = bool ("-optc" <>) id noHs $ gcFlag b
    hsExtra = bool (["--make", "-I.", "-odir", ".atspkg", "-no-hs-main", "-package-db", "~/.cabal/store/ghc-" ++ ghcV' ++ "/package.db/"] ++ packageDbs) mempty noHs
    noHs = null fc
    packageDbs = (\x -> ["-package-db", x ++ "/dist-newstyle/packagedb/ghc-" ++ ghcV']) =<< libToDirs fc

libToDirs :: [ForeignCabal] -> [String]
libToDirs = fmap (takeDirectory . TL.unpack . h)
    where h (ForeignCabal mpr cf _) = fromMaybe cf mpr

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

-- Change architecture to use C stuff better?
-- Step 1: Use patsopt to generate C files
-- Step 2: Use gcc to generate `.o` files
-- Step 3: Generate library/binary from those files.

-- | Location of @patscc@
patscc :: MonadIO m => ATSToolConfig -> m String
patscc = patsTool "patscc"

-- | Location of @patsopt@
patsopt :: MonadIO m => ATSToolConfig -> m String
patsopt = patsTool "patsopt"

patsTool :: MonadIO m => String -> ATSToolConfig -> m String
patsTool tool tc = do
    h <- patsHome (compilerVer tc)
    pure $ h ++ "lib/ats2-postiats-" ++ show (libVersion tc) ++ "/bin/" ++ tool

cconfig :: MonadIO m => ATSToolConfig -> [String] -> Bool -> [String] -> m CConfig
cconfig tc libs gc extras = do
    h <- patsHome (compilerVer tc)
    let cc' = cc tc
    h' <- pkgHome cc'
    home' <- home tc
    let libs' = ("atslib" :) $ bool libs ("gc" : libs) gc
    pure $ CConfig [h ++ "ccomp/runtime/", h, h' ++ "include"] libs' [h' ++ "lib", home' ++ "/ccomp/atslib/lib"] extras

home :: MonadIO m => ATSToolConfig -> m String
home tc = do
    h <- patsHome (compilerVer tc)
    pure $ h ++ "lib/ats2-postiats-" ++ show (libVersion tc)

patsEnv :: FilePath -> FilePath -> [CmdOption]
patsEnv home' path = EchoStderr False :
    zipWith AddEnv
        ["PATSHOME", "PATH", "PATSHOMELOCS"]
        [home', home' ++ "/bin:" ++ path, patsHomeLocs 5]

atsToC :: FilePath -> FilePath
atsToC = (-<.> "c") . (".atspkg/c/" <>)

ghcV :: [ForeignCabal] -> Action String
ghcV hsLibs = case hsLibs of
    [] -> pure undefined
    _  -> ghcVersion

atsBin :: BinaryTarget -> Rules ()
atsBin tgt@BinaryTarget{..} = do

    unless (null genTargets) $
        mapM_ (uncurry3 genATS) genTargets

    unless (null hsLibs) $
        mapM_ cabalExport hsLibs

    let cTargets = atsToC <$> src

    foldr (>>) (pure ()) (atsCGen toolConfig tgt <$> src <*> cTargets)

    binTarget %> \_ -> do

        need cTargets

        ghcV' <- ghcV hsLibs

        cconfig' <- cconfig toolConfig libs gc (makeCFlags cFlags hsLibs ghcV' gc)

        let g Executable    = ccAction
            g StaticLibrary = staticLibA

        unit $ g tgtType (cc toolConfig) cTargets binTarget cconfig'

atsCGen :: ATSToolConfig
        -> BinaryTarget
        -> FilePath -- ^ ATS source
        -> FilePattern -- ^ Pattern for C file to be generated
        -> Rules ()
atsCGen tc BinaryTarget{..} atsSrc cFiles =
    cFiles %> \out -> do

        -- tell shake which files to track and copy them to the appropriate
        -- directory
        need (otherDeps ++ (TL.unpack . objectFile <$> hsLibs))
        sources <- (<> cDeps) <$> transitiveDeps ((^._2) <$> genTargets) [atsSrc]
        need sources
        copySources tc sources

        atsCommand tc atsSrc out

cgen :: ATSToolConfig
     -> [String] -- ^ Additional source files
     -> FilePath -- ^ Directory containing ATS source code
     -> Rules ()
cgen tc extras dir =

    "//*.c" %> \out -> do
        need extras
        let sourceFile = dir ++ "/" ++ (takeBaseName out -<.> "dats")
        handleSource tc sourceFile
        atsCommand tc sourceFile out

handleSource :: ATSToolConfig -> FilePath -> Action ()
handleSource tc sourceFile = do
    sources <- transitiveDeps [] [sourceFile]
    need sources
    copySources tc sources

-- | This provides rules for generating C code from ATS source files in the
trim :: String -> String
trim = init . drop 1

transitiveDeps :: (MonadIO m) => [FilePath] -> [FilePath] -> m [FilePath]
transitiveDeps _ [] = pure []
transitiveDeps gen ps = fmap join $ forM ps $ \p -> if p `elem` gen then pure mempty else do
    contents <- liftIO $ readFile p
    let ats = fromRight mempty . parse $ contents
    let dir = takeDirectory p
    deps <- filterM (\f -> ((f `elem` gen) ||) <$> (liftIO . doesFileExist) f) $ fixDir dir . trim <$> getDependencies ats
    deps' <- transitiveDeps gen deps
    pure $ (p:deps) ++ deps'

-- | This uses @pats-filter@ to prettify the errors.
cgenPretty :: ATSToolConfig
           -> FilePath
           -> Rules ()
cgenPretty tc dir =

    "//*.c" %> \out -> do

        let sourceFile = dir ++ "/" ++ (takeBaseName out -<.> "dats")
        handleSource tc sourceFile
        (Exit c, Stderr err, Stdout _) :: (Exit, Stderr String, Stdout String) <- atsCommand tc sourceFile out
        cmd_ [Stdin err] Shell "pats-filter"
        if c /= ExitSuccess
            then error "patscc failure"
            else pure ()
