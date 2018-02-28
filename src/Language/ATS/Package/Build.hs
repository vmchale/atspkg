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
import           Development.Shake.ATS
import           Development.Shake.C                  (ccFromString)
import           Development.Shake.Check
import           Development.Shake.Clean
import           Development.Shake.Man
import           Distribution.ATS.Version
import           Language.ATS.Package.Compiler
import           Language.ATS.Package.Config
import           Language.ATS.Package.Dependency
import           Language.ATS.Package.Type
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
buildAll p = on (>>) (=<< wants p) fetchDef setupDef
    where fetchDef = fetchCompiler Nothing
          setupDef = setupCompiler Nothing

-- | Build a set of targets
build :: [String] -- ^ Targets
      -> IO ()
build rs = bool (mkPkgEmpty [buildAll Nothing]) (mkPkgEmpty mempty) =<< check Nothing
    where mkPkgEmpty ts = mkPkg False True ts rs Nothing 1

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
        let g str = fmap (((home <> str) <>) . takeFileName)
            binDest =  g "/.local/bin" bins
            libDest = ((home <> "/.atspkg/lib/") <>) . takeFileName <$> libs
            inclDest = ((home <> "/.atspkg/include/") <>) . takeFileName <$> incs
        zipWithM_ copyFile' (bins ++ libs ++ incs) (binDest ++ libDest ++ inclDest)
        pa <- pandoc
        case man config of
            Just mt -> if not pa then pure () else do
                let mt' = manTarget mt
                    manDest = home <> "/.local/share/man/man1/" <> takeFileName mt'
                need [mt']
                copyFile' mt' manDest
            Nothing -> pure ()
        co <- compleat
        case completions config of
            Just com -> if not co then pure () else do
                let com' = unpack com
                    comDest = home <> "/.compleat/" <> takeFileName com'
                need [com']
                copyFile' com' comDest
            Nothing -> pure ()

mkManpage :: Rules ()
mkManpage = do
    c <- getConfig Nothing
    b <- pandoc
    case man c of
        Just _ -> bool (pure ()) manpages b
        _      -> pure ()

cacheConfiguration :: Text -> IO Pkg
cacheConfiguration = input auto

getConfig :: MonadIO m => Maybe FilePath -> m Pkg
getConfig dir' = liftIO $ do
    d <- fromMaybe <$> fmap (<> "/atspkg.dhall") getCurrentDirectory <*> pure dir'
    b <- not <$> doesFileExist ".atspkg/config"
    if b
        then cacheConfiguration (pack d)
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

options :: Bool -- ^ Whether to rebuild all targets
        -> Bool -- ^ Whether to run the linter
        -> Int -- ^ Verbosity level
        -> [String] -- ^ A list of targets
        -> ShakeOptions
options rba lint v rs = shakeOptions { shakeFiles = ".atspkg"
                                     , shakeThreads = 4
                                     , shakeLint = bool Nothing (Just LintBasic) lint
                                     , shakeVersion = showVersion atspkgVersion
                                     , shakeRebuild = rebuildTargets rba rs
                                     , shakeChange = ChangeModtimeAndDigestInput
                                     , shakeVerbosity = toVerbosity v
                                     }

rebuildTargets :: Bool -- ^ Force rebuild of all targets
               -> [String] -- ^ Targets
               -> [(Rebuild, String)]
rebuildTargets rba rs = foldMap g [ (rba, (RebuildNow ,) <$> patterns rs) ]
    where g (b, ts) = bool mempty ts b
          patterns = thread (mkPattern <$> ["c", "o", "so", "a"])
          mkPattern ext = ("//*." <> ext :)

cleanConfig :: (MonadIO m) => [String] -> m Pkg
cleanConfig ["clean"] = pure undefined
cleanConfig _         = getConfig Nothing

mkPkg :: Bool -- ^ Force rebuild
      -> Bool -- ^ Run linter
      -> [IO ()] -- ^ Setup
      -> [String] -- ^ Targets
      -> Maybe String -- ^ Target triple
      -> Int -- ^ Verbosity
      -> IO ()
mkPkg rba lint setup rs tgt v = do
    cfg <- cleanConfig rs
    let opt = options rba lint v $ pkgToTargets cfg rs
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
        liftIO $ BSL.writeFile out (encode (x :: Pkg))

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

noConstr :: ATSConstraint
noConstr = ATSConstraint Nothing Nothing

pkgToAction :: [IO ()] -- ^ Setup actions to be performed
            -> [String] -- ^ Targets
            -> Maybe String -- ^ Optional compiler triple (overrides 'ccompiler')
            -> Pkg -- ^ Package data type
            -> Rules ()
pkgToAction setup rs tgt ~(Pkg bs ts libs mt _ v v' ds cds bdeps ccLocal cf as) =

    unless (rs == ["clean"]) $ do

        let cdps = if f bs || f ts then ("gc", noConstr) : cds else cds where f = any gcBin

        mkUserConfig

        ".atspkg/deps" %> \out -> do
            (_, cfgBin') <- cfgBin
            need [ cfgBin', ".atspkg/config" ]
            liftIO $ fetchDeps (ccFromString cc') setup (unpack . fst <$> ds) (unpack . fst <$> cdps) (unpack . fst <$> bdeps) cfgBin' False >> writeFile out ""

        let bins = unpack . target <$> bs
        setTargets rs bins mt

        cDepsRules >> bits rs

        mapM_ h libs

        mapM_ g (bs ++ ts)

    where g (Bin s t ls hs' atg gc' extra) =
            atsBin (ATSTarget (unpack <$> cf) atsToolConfig gc' (unpack <$> ls) [unpack s] hs' (unpackBoth . asTuple <$> atg) mempty (unpack t) (deps extra) Executable)

          h (Lib _ s t ls _ hs' lnk atg extra sta) =
            atsBin (ATSTarget (unpack <$> cf) atsToolConfig False (unpack <$> ls) (unpack <$> s) hs' (unpackBoth . asTuple <$> atg) (both unpack <$> lnk) (unpack t) (deps extra) (k sta))

          k False = SharedLibrary
          k True  = StaticLibrary

          atsToolConfig = ATSToolConfig v v' False (ccFromString cc')

          cDepsRules = unless (null as) $ do
              let targets = fmap (unpack . cTarget) as
                  sources = fmap (unpack . atsSrc) as
                  l x = ATSTarget undefined atsToolConfig False mempty mempty mempty x mempty undefined [".atskg/deps", ".atspkg/config"] undefined
              zipWithM_ (cgen (l (unpackBoth . asTuple <$> (atsGen =<< as)))) sources targets

          cc' = maybe (unpack ccLocal) (<> "-gcc") tgt
          deps = (".atspkg/deps":) . (".atspkg/config":) . fmap unpack

          unpackBoth :: (Text, Text, Bool) -> (String, String, Bool)
          unpackBoth = over _1 unpack . over _2 unpack
