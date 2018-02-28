{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Development.Shake.ATS ( -- * Shake Rules
                               cleanATS
                             , atsBin
                             , atsLex
                             , cabalExport
                             , cgen
                             , genATS
                             -- * Helper functions
                             , getSubdirs
                             , ccToDir
                             , withPF
                             -- * Environment/configuration
                             , patscc
                             , patsopt
                             -- Types
                             , ForeignCabal (..)
                             , ATSTarget (..)
                             , ATSToolConfig (..)
                             , CCompiler (..)
                             , ArtifactType (..)
                             ) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bool                         (bool)
import           Data.Either                       (fromRight)
import           Data.List                         (intercalate)
import           Data.Maybe                        (fromMaybe)
import           Data.Semigroup                    (Semigroup (..))
import qualified Data.Text.Lazy                    as TL
import           Development.Shake                 hiding (doesFileExist, getEnv)
import           Development.Shake.ATS.Environment
import           Development.Shake.ATS.Rules
import           Development.Shake.ATS.Type
import           Development.Shake.C
import           Development.Shake.FilePath
import           Development.Shake.Version
import           Language.ATS
import           Lens.Micro
import           System.Directory                  (copyFile, createDirectoryIfMissing, doesFileExist)
import           System.Environment                (getEnv)
import           System.Exit                       (ExitCode (ExitSuccess))

-- | Run @patsopt@ given information about various things
atsCommand :: CmdResult r => ATSToolConfig
                          -> String -- ^ Source file
                          -> String -- ^ C code to be generated
                          -> Action r
atsCommand tc sourceFile out = do
    path <- liftIO $ getEnv "PATH"
    home' <- home tc
    let env = patsEnv home' path
    patsc <- patsopt tc

    command env patsc ["--output", out, "-dd", sourceFile, "-cc"]

-- | Filter any generated errors with @pats-filter@.
withPF :: Bool -> Action (Exit, Stderr String, Stdout String) -> Action (Exit, Stderr String, Stdout String)
withPF False = id
withPF True = \act -> do
    ret@(Exit c, Stderr err, Stdout _) <- act :: Action (Exit, Stderr String, Stdout String)
    cmd_ [Stdin err] Shell "pats-filter"
    if c /= ExitSuccess
        then error "patsopt failure"
        else pure ret

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

-- | This is the @$PATSHOMELOCS@ variable to be passed to the shell.
patsHomeLocs :: Int
             -> String
patsHomeLocs n = intercalate ":" $ (<> ".atspkg/contrib") . ("./" <>) <$> g
    where g = [ join $ replicate i "../" | i <- [0..n] ]

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

-- | Location of @patscc@
patscc :: MonadIO m => ATSToolConfig -> m String
patscc = patsTool "patscc"

-- | Location of @patsopt@
patsopt :: MonadIO m => ATSToolConfig -> m String
patsopt = patsTool "patsopt"

patsTool :: MonadIO m => String -> ATSToolConfig -> m String
patsTool tool tc = (<> prep) <$> ph
    where ph = patsHome (compilerVer tc)
          prep = "lib/ats2-postiats-" ++ show (libVersion tc) ++ "/bin/" ++ tool

cconfig :: MonadIO m => ATSToolConfig -> [String] -> Bool -> [String] -> m CConfig
cconfig tc libs gc extras = do
    h <- patsHome (compilerVer tc)
    let cc' = cc tc
    h' <- pkgHome cc'
    home' <- home tc
    let libs' = ("atslib" :) $ bool libs ("gc" : libs) gc
    pure $ CConfig [h ++ "ccomp/runtime/", h, h' ++ "include", ".atspkg/contrib"] libs' [h' ++ "lib", home' ++ "/ccomp/atslib/lib"] extras

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

doLib :: ArtifactType -> Rules () -> Rules ()
doLib Executable = pure mempty
doLib _          = id

atsBin :: ATSTarget -> Rules ()
atsBin ATSTarget{..} = do

    mapM_ (uncurry genLinks) linkTargets

    mapM_ (uncurry3 genATS) genTargets

    mapM_ cabalExport hsLibs

    let cTargets = atsToC <$> src

    let h Executable    = id
        h StaticLibrary = fmap (-<.> "o")
        h SharedLibrary = fmap (-<.> "o")
        g Executable    = ccAction
        g StaticLibrary = staticLibA
        g SharedLibrary = sharedLibA
        h' = h tgtType

    cconfig' <- cconfig toolConfig libs gc (makeCFlags cFlags mempty (pure undefined) gc)

    let atsGen = (snd <$> linkTargets) <> ((^._2) <$> genTargets)
        atsExtras = otherDeps <> (TL.unpack . objectFile <$> hsLibs)
    zipWithM_ (cgen toolConfig atsExtras atsGen) src cTargets

    doLib tgtType (zipWithM_ (objectFileR (cc toolConfig) cconfig') cTargets (h' cTargets))

    binTarget %> \_ -> do

        need (h' cTargets)

        ghcV' <- ghcV hsLibs

        cconfig'' <- cconfig toolConfig libs gc (makeCFlags cFlags hsLibs ghcV' gc)

        unit $ g tgtType (cc toolConfig) (h' cTargets) binTarget cconfig''

cgen :: ATSToolConfig
     -> [FilePath] -- ^ Extra files to track
     -> [FilePath] -- ^ ATS source that may be generated.
     -> FilePath -- ^ ATS source
     -> FilePattern -- ^ Pattern for C file to be generated
     -> Rules ()
cgen toolConfig extras atsGens atsSrc cFiles =
    cFiles %> \out -> do

        -- tell shake which files to track and copy them to the appropriate
        -- directory
        need extras
        sources <- transitiveDeps atsGens [atsSrc]
        need sources
        copySources toolConfig sources

        atsCommand toolConfig atsSrc out

-- | This provides rules for generating C code from ATS source files in the
trim :: String -> String
trim = init . drop 1

-- | Print any errors to standard error.
maybeError :: (MonadIO m) => Either ATSError b -> m ()
maybeError Right{}  = pure ()
maybeError (Left y) = warnErr y

transitiveDeps :: (MonadIO m) => [FilePath] -> [FilePath] -> m [FilePath]
transitiveDeps _ [] = pure []
transitiveDeps gen ps = fmap join $ forM ps $ \p -> if p `elem` gen then pure mempty else do
    contents <- liftIO $ readFile p
    let (ats, err) = (fromRight mempty &&& maybeError) . parse $ contents
    err
    let dir = takeDirectory p
    deps <- filterM (\f -> ((f `elem` gen) ||) <$> (liftIO . doesFileExist) f) $ fixDir dir . trim <$> getDependencies ats
    deps' <- transitiveDeps gen deps
    pure $ (p:deps) ++ deps'
