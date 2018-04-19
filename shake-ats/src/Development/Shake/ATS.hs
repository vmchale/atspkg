{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Development.Shake.ATS ( -- * Shake Rules
                               atsBin
                             , cgen
                             , genATS
                             , atsLex
                             , cabalForeign
                             , hsAts
                             -- * Shake actions
                             , cleanATS
                             -- * Helper functions
                             , getSubdirs
                             , ccToDir
                             , withPF
                             -- * Environment/configuration
                             , patscc
                             , patsopt
                             -- * Types
                             , ForeignCabal (..)
                             , ATSTarget (..)
                             , ATSToolConfig (..)
                             , CCompiler (..)
                             , ArtifactType (..)
                             , ATSGen (..)
                             , HATSGen (..)
                             , Solver (..)
                             -- * Lenses
                             , atsTarget
                             , cFlags
                             , binTarget
                             , cc
                             , gc
                             , hasPretty
                             , genTargets
                             , hsLibs
                             , patsHome
                             , patsHomeLocs
                             , libs
                             , linkStatic
                             , linkTargets
                             , otherDeps
                             , src
                             , tgtType
                             , toolConfig
                             , cpphs
                             , hsFile
                             , strip
                             , solver
                             , linkATSLib
                             ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bool                         (bool)
import           Data.Either                       (fromRight)
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
    let env = patsEnv tc path
        patsc = patsopt tc
        f = case _solver tc of
                Ignore -> ("--constraint-ignore":)
                _      -> id

    command env patsc (f ["--output", out, "-dd", sourceFile, "-cc"])

-- | Filter any generated errors with @pats-filter@.
withPF :: Action (Exit, Stderr String, Stdout String) -- ^ Result of a 'cmd' or 'command'
       -> Action (Exit, Stderr String, Stdout String)
withPF act = do
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
copySources (ATSToolConfig home' _ _ _ _ _ _) sources =
    forM_ sources $ \dep -> do
        liftIO $ createDirectoryIfMissing True (home' ++ "/" ++ takeDirectory dep)
        liftIO $ copyFile dep (home' ++ "/" ++ dep)

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

-- | Absolute path to @patscc@
patscc :: ATSToolConfig -> String
patscc = patsTool "patscc"

-- | Absolute path to @patsopt@
patsopt :: ATSToolConfig -> String
patsopt = patsTool "patsopt"

patsTool :: String -> ATSToolConfig -> String
patsTool tool tc = _patsHome tc ++ "/bin/" ++ tool

cconfig :: MonadIO m => ATSToolConfig -> [String] -> Bool -> [String] -> m CConfig
cconfig tc libs' gc' extras = do
    let h = _patsHome tc
        cc' = _cc tc
        f = bool id ("atslib":) (_linkATSLib tc)
    h' <- pkgHome cc'
    -- FIXME only bother with atslib if it's unnecessary?
    let libs'' = f $ bool libs' ("gc" : libs') gc'
    pure $ CConfig [h ++ "/ccomp/runtime/", h, h' ++ "include", ".atspkg/contrib"] libs'' [h' ++ "lib", _patsHome tc ++ "/ccomp/atslib/lib"] extras (_linkStatic tc)

patsEnv :: ATSToolConfig -> FilePath -> [CmdOption]
patsEnv cfg path = EchoStderr False :
    zipWith AddEnv
        ["PATSHOME", "PATH", "PATSHOMELOCS", "LIBGMP"]
        [_patsHome cfg, _patsHome cfg ++ "/bin:" ++ path, _patsHomeLocs cfg ]

atsToC :: FilePath -> FilePath
atsToC = (-<.> "c") . (".atspkg/c/" <>)

ghcV :: [ForeignCabal] -> Action String
ghcV hsLibs' = case hsLibs' of
    [] -> pure undefined
    _  -> ghcVersion

doLib :: ArtifactType -> Rules () -> Rules ()
doLib Executable = pure mempty
doLib _          = id

hsAts :: ATSGen -> Rules ()
hsAts (ATSGen x y z) = genATS x y z

satsGen :: HATSGen -> Rules ()
satsGen (HATSGen x y) = genLinks x y

-- /.atspkg/contrib/atscntrb-hx-libgmp

-- | Rules for generating binaries or libraries from ATS code. This is very
-- general; use 'defaultATSTarget' for sensible defaults that can be modified
-- with the provided lenses.
atsBin :: ATSTarget -> Rules ()
atsBin ATSTarget{..} = do

    mapM_ satsGen _linkTargets

    mapM_ hsAts _genTargets

    mapM_ cabalForeign _hsLibs

    let cTargets = atsToC <$> _src

    let h Executable    = id
        h StaticLibrary = fmap (-<.> "o")
        h SharedLibrary = fmap (-<.> "o")
        g Executable    = binaryA
        g StaticLibrary = staticLibA
        g SharedLibrary = sharedLibA
        h' = h _tgtType

    cconfig' <- cconfig _toolConfig _libs _gc (makeCFlags _cFlags mempty (pure undefined) _gc)

    let atsGen = (hatsFile <$> _linkTargets) <> ((^.atsTarget) <$> _genTargets)
        atsExtras = _otherDeps <> (TL.unpack . objectFile <$> _hsLibs)
    zipWithM_ (cgen _toolConfig atsExtras atsGen) _src cTargets

    doLib _tgtType (zipWithM_ (objectFileR (_cc _toolConfig) cconfig') cTargets (h' cTargets))

    _binTarget %> \_ -> do

        need (h' cTargets)

        ghcV' <- ghcV _hsLibs

        cconfig'' <- cconfig _toolConfig _libs _gc (makeCFlags _cFlags _hsLibs ghcV' _gc)

        unit $ g _tgtType (_cc _toolConfig) (h' cTargets) _binTarget cconfig''
        bool (pure ()) (stripA _binTarget (_cc _toolConfig)) _strip

-- | Generate C code from ATS code.
cgen :: ATSToolConfig
     -> [FilePath] -- ^ Extra files to track
     -> [FilePath] -- ^ ATS source that may be generated.
     -> FilePath -- ^ ATS source
     -> FilePattern -- ^ Pattern for C file to be generated
     -> Rules ()
cgen toolConfig' extras atsGens atsSrc cFiles =
    cFiles %> \out -> do

        -- tell shake which files to track and copy them to the appropriate
        -- directory
        need extras
        sources <- transitiveDeps atsGens [atsSrc]
        need sources
        copySources toolConfig' sources

        atsCommand toolConfig' atsSrc out

-- | This provides rules for generating C code from ATS source files in the
trim :: String -> String
trim = init . drop 1

-- | Print any errors to standard error.
maybeError :: (MonadIO m) => FilePath -> Either ATSError b -> m ()
maybeError _ Right{}  = pure ()
maybeError p (Left y) = warnErr p y

transitiveDeps :: (MonadIO m) => [FilePath] -> [FilePath] -> m [FilePath]
transitiveDeps _ [] = pure []
transitiveDeps gen ps = fmap join $ forM ps $ \p -> if p `elem` gen then pure mempty else do
    contents <- liftIO $ readFile p
    let (ats, err) = (fromRight mempty &&& maybeError p) . parseM $ contents
    err
    let dir = takeDirectory p
    deps <- filterM (\f -> ((f `elem` gen) ||) <$> (liftIO . doesFileExist) f) $ fixDir dir . trim <$> getDependencies ats
    deps' <- transitiveDeps gen deps
    pure $ (p:deps) ++ deps'
