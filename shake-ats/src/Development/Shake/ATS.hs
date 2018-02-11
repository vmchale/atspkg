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
import           Development.Shake
import           Development.Shake.ATS.Environment
import           Development.Shake.ATS.Rules
import           Development.Shake.ATS.Type
import           Development.Shake.FilePath
import           Development.Shake.Version
import           Language.ATS
import           Lens.Micro
import           System.Directory                  (copyFile, createDirectoryIfMissing)
import           System.Exit                       (ExitCode (ExitSuccess))

-- | Whether generated libraries are to be considered compatible.
compatible :: CCompiler -> CCompiler -> Bool
compatible (GCC Nothing Nothing) Clang = True
compatible Clang (GCC Nothing Nothing) = True
compatible x y                         = x == y

-- | Run @patsopt@ given information about
atsCommand :: CmdResult r => ATSToolConfig
                          -> String -- ^ Source file
                          -> String -- ^ C code to be generated
                          -> Action r
atsCommand tc sourceFile out = do
    h <- patsHome (compilerVer tc)
    let home = h ++ "lib/ats2-postiats-" ++ show (libVersion tc)
        atsArgs = [EchoStderr False, AddEnv "PATSHOME" home]
        patsc = home ++ "/bin/patsopt"
    command atsArgs patsc ["--output", out, "-dd", sourceFile, "-cc"]

gcFlag :: Bool -> String
gcFlag False = "-DATS_MEMALLOC_LIBC"
gcFlag True  = "-DATS_MEMALLOC_GCBDW"

-- Copy source files to the appropriate place. This is necessary because
-- @#include@s in ATS are weird.
copySources :: ATSToolConfig -> [FilePath] -> Action ()
copySources (ATSToolConfig v v' _ _) sources =
    forM_ sources $ \dep -> do
        h <- patsHome v'
        let home = h ++ "lib/ats2-postiats-" ++ show v
        liftIO $ createDirectoryIfMissing True (home ++ "/" ++ takeDirectory dep)
        liftIO $ copyFile dep (home ++ "/" ++ dep)

-- This is the @$PATSHOMELOCS@ variable to be passed to the shell.
patsHomeLocs :: Int -> String
patsHomeLocs n = intercalate ":" $ (<> ".atspkg/contrib") . ("./" <>) <$> g
    where g = [ join $ replicate i "../" | i <- [1..n] ]

-- TODO depend on GHC version?
makeCFlags :: [String] -- ^ Inputs
           -> [ForeignCabal] -- ^ Haskell libraries
           -> String -- ^ GHC version
           -> Bool -- ^ Whether to use the Garbage collector
           -> [String]
makeCFlags ss fc ghcV b = gcFlag' : (hsExtra <> ss) where
    gcFlag' = bool ("-optc" <>) id noHs $ gcFlag b
    hsExtra = bool (["--make", "-odir", ".atspkg", "-no-hs-main", "-package-db", "~/.cabal/store/ghc-" ++ ghcV ++ "/package.db/"] ++ packageDbs) mempty noHs
    noHs = null fc
    packageDbs = (\x -> ["-package-db", x ++ "/dist-newstyle/packagedb/ghc-" ++ ghcV]) =<< libToDirs fc

libToDirs :: [ForeignCabal] -> [String]
libToDirs = fmap (takeDirectory . TL.unpack . h)
    where h (ForeignCabal mpr cf _) = fromMaybe cf mpr

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

-- TODO libraries should be linked against *cross-compiled* versions!!
-- aka we need to compile atslib
atsBin :: BinaryTarget -> Rules ()
atsBin BinaryTarget{..} = do

    unless (null genTargets) $
        mapM_ (uncurry3 genATS) genTargets

    unless (null hsLibs) $
        mapM_ cabalExport hsLibs

    binTarget %> \_ -> do
        h <- patsHome (compilerVer toolConfig)
        let cc' = cc toolConfig
        h' <- pkgHome cc'
        let home = h ++ "lib/ats2-postiats-" ++ show (libVersion toolConfig)
        sources <- (<> otherDeps <>) . (<> cDeps) <$> transitiveDeps ((^._2) <$> genTargets) [src]
        b' <- doesFileExist "atspkg.dhall"
        let hb = bool id ("atspkg.dhall" :) b'
        need (hb (sources ++ (TL.unpack . objectFile <$> hsLibs)))
        copySources toolConfig sources

        let ccommand = unwords [ ccToString cc', "-I" ++ h ++ "ccomp/runtime/", "-I" ++ h, "-I" ++ h' ++ "include", "-L" ++  h' ++ "lib", "-L" ++ home ++ "/ccomp/atslib/lib"]
        path <- fromMaybe "" <$> getEnv "PATH"
        let toLibs = fmap ("-l" <>)
        let libs' = ("atslib" :) $ bool libs ("gc" : libs) gc
        ghcV <- case hsLibs of
            [] -> pure undefined
            _  -> ghcVersion
        command_
            [EchoStderr False, AddEnv "PATSHOME" home, AddEnv "PATH" (home ++ "/bin:" ++ path), AddEnv "PATSHOMELOCS" $ patsHomeLocs 5]
            (home ++ "/bin/patscc")
            (mconcat
                [ [src, "-atsccomp", ccommand, "-o", binTarget, "-cleanaft"]
                , makeCFlags cFlags hsLibs ghcV gc
                , toLibs libs'
                ])

-- given directory.
cgen :: ATSToolConfig
     -> FilePath -- ^ Directory containing ATS source code
     -> Rules ()
cgen tc dir =

    "//*.c" %> \out -> do
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

transitiveDeps :: [FilePath] -> [FilePath] -> Action [FilePath]
transitiveDeps _ [] = pure []
transitiveDeps gen ps = fmap join $ forM ps $ \p -> if p `elem` gen then pure mempty else do
    contents <- liftIO $ readFile p
    let ats = fromRight mempty . parse $ contents
    let dir = takeDirectory p
    deps <- filterM (\f -> ((f `elem` gen) ||) <$> doesFileExist f) $ fixDir dir . trim <$> getDependencies ats
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
