{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- | This module holds various functions for turning a package into a set of rules
-- or an 'IO ()'.
module Language.ATS.Package.Build ( mkPkg
                                  , pkgToAction
                                  , build
                                  , buildAll
                                  , check
                                  ) where

import           Control.Concurrent.ParallelIO.Global
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BSL
import           Data.List                            (nub)
import           Data.Version                         (showVersion)
import           Development.Shake.ATS
import           Development.Shake.Check
import           Development.Shake.Clean
import           Development.Shake.Man
import           Language.ATS.Package.Compiler
import           Language.ATS.Package.Config
import           Language.ATS.Package.Dependency
import           Language.ATS.Package.Type            hiding (Version)
import qualified Paths_ats_pkg                        as P
import           Quaalude

check :: Maybe FilePath -> IO Bool
check p = do
    home <- getEnv "HOME"
    v <- wants p
    doesFileExist (home ++ "/.atspkg/" ++ show v ++ "/bin/patscc")

wants :: Maybe FilePath -> IO Version
wants p = compiler <$> getConfig p

-- | Build in current directory or indicated directory
buildAll :: Maybe FilePath -> IO ()
buildAll p = on (>>) (=<< wants p) fetchCompiler setupCompiler

-- | Build a set of targets
build :: [String] -- ^ Targets
      -> IO ()
build rs = bool (mkPkgEmpty [buildAll Nothing]) (mkPkgEmpty mempty) =<< check Nothing
    where mkPkgEmpty ts = mkPkg False False True ts rs Nothing 1

-- TODO clean generated ATS
mkClean :: Rules ()
mkClean = "clean" ~> do
    cleanHaskell
    removeFilesAfter "." ["//*.1", "//*.c", "tags", "//*.a"]
    removeFilesAfter "target" ["//*"]
    removeFilesAfter ".atspkg" ["//*"]
    removeFilesAfter "ats-deps" ["//*"]

mkInstall :: Rules ()
mkInstall =
    "install" ~> do
        config <- getConfig Nothing
        let libs = fmap (unpack . libTarget) . libraries $ config
            bins = fmap (unpack . target) . bin $ config
            incs = ((fmap unpack . includes) =<<) . libraries $ config
        need (bins <> libs)
        home <- liftIO $ getEnv "HOME"
        let binDest = ((home <> "/.local/bin/") <>) . takeFileName <$> bins
        let libDest = ((home <> "/.atspkg/lib/") <>) . takeFileName <$> libs
        let inclDest = ((home <> "/.atspkg/includes/") <>) . takeFileName <$> incs
        zipWithM_ copyFile' (bins ++ libs ++ incs) (binDest ++ libDest ++ inclDest)
        pa <- pandoc
        case man config of
            Just mt -> if not pa then pure () else do
                let mt' = manTarget mt
                    manDest = home <> "/.local/share/man/man1/" <> takeFileName mt'
                need [mt']
                copyFile' mt' manDest
            Nothing -> pure ()

mkManpage :: Rules ()
mkManpage = do
    c <- getConfig Nothing
    b <- pandoc
    case man c of
        Just _ -> bool (pure ()) manpages b
        _      -> pure ()

-- TODO allow it to be called in parent directory
-- getParents :: FilePath -> IO [FilePath]
-- getParents p = do

getConfig :: MonadIO m => Maybe FilePath -> m Pkg
getConfig dir' = liftIO $ do
    d <- fromMaybe <$> fmap (<> "/atspkg.dhall") getCurrentDirectory <*> pure dir'
    b <- not <$> doesFileExist ".atspkg/config"
    if b
        then input auto (pack d)
        else fmap (decode . BSL.fromStrict) . BS.readFile $ ".atspkg/config"

manTarget :: Text -> FilePath
manTarget m = unpack m -<.> "1"

mkPhony :: String -> (String -> String) -> (Pkg -> [Bin]) -> [String] -> Rules ()
mkPhony cmdStr f select rs =
    cmdStr ~> do
        config <- getConfig Nothing
        let runs = bool (filter (/= cmdStr) rs) (fmap (unpack . target) . select $ config) (rs == [cmdStr])
        need runs
        mapM_ cmd_ (f <$> runs)

mkValgrind :: [String] -> Rules ()
mkValgrind = mkPhony "valgrind" ("valgrind " <>) bin

mkTest :: [String] -> Rules ()
mkTest = mkPhony "test" id test

mkRun :: [String] -> Rules ()
mkRun = mkPhony "run" id bin

toVerbosity :: Int -> Verbosity
toVerbosity 0 = Normal
toVerbosity 1 = Loud
toVerbosity 2 = Chatty
toVerbosity 3 = Diagnostic
toVerbosity _ = Diagnostic -- really should be a warning

options :: Bool -- ^ Whether to rebuild config
        -> Bool -- ^ Whether to rebuild all targets
        -> Bool -- ^ Whether to run the linter
        -> Int -- ^ Verbosity level
        -> [String] -- ^ A list of targets
        -> ShakeOptions
options rb rba lint v rs = shakeOptions { shakeFiles = ".atspkg"
                          , shakeThreads = 4
                          , shakeLint = bool Nothing (Just LintBasic) lint
                          , shakeVersion = showVersion P.version
                          , shakeRebuild = foldMap g [ (rb, [(RebuildNow, ".atspkg/config")])
                                                     , (rba, (RebuildNow ,) <$> rs)
                                                     ]
                          , shakeChange = ChangeModtimeAndDigestInput
                          , shakeVerbosity = toVerbosity v
                          }
    where g (b, ts) = bool mempty ts b

cleanConfig :: (MonadIO m) => [String] -> m Pkg
cleanConfig ["clean"] = pure undefined
cleanConfig _         = getConfig Nothing

mkPkg :: Bool -- ^ Whether to ignore cached package config
      -> Bool -- ^ Rebuild all targets
      -> Bool -- ^ Run linter
      -> [IO ()] -- ^ Setup
      -> [String] -- ^ Targets
      -> Maybe String -- ^ Target triple
      -> Int -- ^ Verbosity
      -> IO ()
mkPkg rb rba lint setup rs tgt v = do
    cfg <- cleanConfig rs
    let opt = options rb rba lint v $ pkgToTargets cfg rs
    shake opt $
        mconcat
            [ want (pkgToTargets cfg rs)
            , mkClean
            , pkgToAction setup rs tgt =<< cleanConfig rs
            ]
    stopGlobalPool

asTuple :: TargetPair -> (Text, Text, Bool)
asTuple (TargetPair s t b) = (s, t, b)

mkConfig :: Rules ()
mkConfig =
    ".atspkg/config" %> \out -> do
        need ["atspkg.dhall"]
        x <- liftIO $ input auto "./atspkg.dhall"
        let bts = encode (x :: Pkg)
        liftIO $ BSL.writeFile out bts

setTargets :: [String] -> [FilePath] -> Maybe Text -> Rules ()
setTargets rs bins mt = when (null rs) $
    case mt of
        (Just m) -> want . bool bins (manTarget m : bins) =<< pandoc
        Nothing  -> want bins

bits :: [String] -> Rules ()
bits rs = mconcat $ [ mkManpage, mkInstall, mkConfig ] <>
    sequence [ mkRun, mkTest, mkValgrind ] rs

pkgToTargets :: Pkg -> [FilePath] -> [FilePath]
pkgToTargets ~Pkg{..} [] = (unpack . target <$> bin) <> (unpack . libTarget <$> libraries)
pkgToTargets _ ts        = ts

pkgToAction :: [IO ()] -- ^ Setup actions to be performed
            -> [String] -- ^ Targets
            -> Maybe String -- ^ Optional compiler triple (overrides 'ccompiler')
            -> Pkg -- ^ Package data type
            -> Rules ()
pkgToAction setup rs tgt ~(Pkg bs ts libs mt v v' ds cds bdeps ccLocal cf as cdir) =

    unless (rs == ["clean"]) $ do

        let cdps = if f bs || f ts then "gc" : cds else cds where f = any gcBin

        mkUserConfig

        ".atspkg/deps" %> \out -> do
            (_, cfgBin') <- cfgBin
            need [ cfgBin' ]
            liftIO $ fetchDeps (ccFromString cc') setup (unpack <$> ds) (unpack <$> cdps) (unpack <$> bdeps) cfgBin' False >> writeFile out ""

        let bins = unpack . target <$> bs
        setTargets rs bins mt

        cDepsRules >> bits rs

        mapM_ h libs

        mapM_ g (bs ++ ts)

    where g (Bin s t ls hs' atg gc' cSrc extra) =
            atsBin (BinaryTarget (unpack <$> cf) atsToolConfig gc' (unpack <$> ls) [unpack s] hs' (unpackBoth . asTuple <$> atg) (unpack t) (unpack <$> cSrc) (deps extra) Executable)

          h (Lib _ s t ls _ hs' atg cSrc extra) =
            atsBin (BinaryTarget (unpack <$> cf) atsToolConfig False (unpack <$> ls) (unpack <$> s) hs' (unpackBoth . asTuple <$> atg) (unpack t) (unpack <$> cSrc) (deps extra) StaticLibrary)

          atsToolConfig = ATSToolConfig v v' False (ccFromString cc')

          cDepsRules = unless (null as) $ do
            let cedar = unpack cdir
                atsSourceDirs = nub (takeDirectory . unpack <$> as)
                targets = fmap (((cedar <> "/") <>) . (-<.> "c") . takeBaseName . unpack) as
            want targets
            hasPF <- patsFilter
            mapM_ (cgen (ATSToolConfig v v' hasPF (ccFromString cc')) [".atspkg/deps", ".atspkg/config"]) atsSourceDirs

          cc' = maybe (unpack ccLocal) (<> "-gcc") tgt
          deps = (".atspkg/deps":) . (".atspkg/config":) . fmap unpack

          unpackBoth :: (Text, Text, Bool) -> (String, String, Bool)
          unpackBoth = over _1 unpack . over _2 unpack
