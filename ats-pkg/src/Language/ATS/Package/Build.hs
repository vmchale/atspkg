{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- | This module holds various functions for turning a package into a set of rules
-- or an 'IO ()'.
module Language.ATS.Package.Build ( mkPkg
                                  , build
                                  , buildAll
                                  , check
                                  ) where

import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as BSL
import           Data.List                       (intercalate)
import           Development.Shake               (alwaysRerun, getVerbosity)
import           Development.Shake.ATS
import           Development.Shake.C             (ccFromString)
import           Development.Shake.Check
import           Development.Shake.Clean
import           Development.Shake.Man
import           Distribution.ATS.Version
import           Language.ATS.Package.Build.C
import           Language.ATS.Package.Compiler
import           Language.ATS.Package.Config
import           Language.ATS.Package.Debian     hiding (libraries, target)
import           Language.ATS.Package.Dependency
import           Language.ATS.Package.Type
import           Quaalude

check :: Maybe FilePath -> IO Bool
check p = do
    home <- getEnv "HOME"
    v <- wants p
    doesFileExist (home </> ".atspkg" </> show v </> "bin" </> "patscc")

wants :: Maybe FilePath -> IO Version
wants p = compiler <$> getConfig p

-- | Build in current directory or indicated directory
buildAll :: Int
         -> Maybe String
         -> Maybe FilePath
         -> IO ()
buildAll v tgt' p = on (>>) (=<< wants p) fetchDef setupDef
    where fetchDef = fetchCompiler
          setupDef = setupCompiler (toVerbosity v) atslibSetup tgt'

build' :: FilePath -- ^ Directory
       -> Maybe String -- ^ Target triple
       -> [String] -- ^ Targets
       -> IO ()
build' dir tgt' rs = withCurrentDirectory dir (mkPkgEmpty mempty)
    where mkPkgEmpty ts = mkPkg False True False ts rs tgt' 1

-- | Build a set of targets
build :: Int
      -> [String] -- ^ Targets
      -> IO ()
build v rs = bool (mkPkgEmpty [buildAll v Nothing Nothing]) (mkPkgEmpty mempty) =<< check Nothing
    where mkPkgEmpty ts = mkPkg False True False ts rs Nothing 1

-- TODO clean generated ATS
mkClean :: Rules ()
mkClean = "clean" ~> do
    cleanHaskell
    removeFilesAfter "." ["//*.1", "//*.c", "tags", "//*.a"]
    removeFilesAfter "target" ["//*"]
    removeFilesAfter ".atspkg" ["//*"]
    removeFilesAfter "ats-deps" ["//*"]

-- TODO take more arguments, in particular, include + library dirs
mkInstall :: Maybe String -- ^ Optional target triple
          -> Rules ()
mkInstall tgt =
    "install" ~> do
        config <- getConfig Nothing
        let libs' = fmap (unpack . libTarget) . libraries $ config
            bins = fmap (unpack . target) . bin $ config
            incs = ((fmap unpack . includes) =<<) . libraries $ config
            libDir = maybe mempty (<> [pathSeparator]) tgt
        need (bins <> libs')
        home <- liftIO $ getEnv "HOME"
        let g str = fmap (((home </> str) </>) . takeFileName)
            binDest =  g (".local" </> "bin") bins
            libDest = ((home </> ".atspkg" </> libDir </> "lib") </>) . takeFileName <$> libs'
            inclDest = ((home </> ".atspkg" </> "include") </>) . takeFileName <$> incs
        zipWithM_ copyFile' (bins ++ libs' ++ incs) (binDest ++ libDest ++ inclDest)
        pa <- pandoc
        case man config of
            Just mt -> if not pa then pure () else do
                let mt' = manTarget mt
                    manDest = home </> ".local" </> "share" </> "man" </> "man1" </> takeFileName mt'
                need [mt']
                copyFile' mt' manDest
            Nothing -> pure ()
        co <- compleat
        case completions config of
            Just com -> if not co then pure () else do
                let com' = unpack com
                    comDest = home </> ".compleat" </> takeFileName com'
                need [com'] -- FIXME do this all in one step
                copyFile' com' comDest
            Nothing -> pure ()

mkManpage :: Rules ()
mkManpage = do
    c <- getConfig Nothing
    b <- pandoc
    case man c of
        Just _ -> bool (pure ()) manpages b
        _      -> pure ()

-- FIXME this doesn't rebuild when it should; it should rebuild when
-- @atspkg.dhall@ changes.
getConfig :: MonadIO m => Maybe FilePath -> m Pkg
getConfig dir' = liftIO $ do
    d <- fromMaybe <$> fmap (</> "atspkg.dhall") getCurrentDirectory <*> pure dir'
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
toVerbosity _ = Diagnostic -- should be a warning

options :: Bool -- ^ Whether to rebuild all targets
        -> Bool -- ^ Whether to run the linter
        -> Bool -- ^ Whether to display profiling information for the build
        -> Int -- ^ Verbosity level
        -> [String] -- ^ A list of targets
        -> ShakeOptions
options rba lint tim v rs = shakeOptions { shakeFiles = ".atspkg"
                                         , shakeThreads = 4
                                         , shakeLint = bool Nothing (Just LintBasic) lint
                                         , shakeVersion = showVersion atspkgVersion
                                         , shakeRebuild = rebuildTargets rba rs
                                         , shakeChange = ChangeModtimeAndDigestInput
                                         , shakeVerbosity = toVerbosity v
                                         , shakeTimings = tim
                                         }

rebuildTargets :: Bool -- ^ Force rebuild of all targets
               -> [String] -- ^ Targets
               -> [(Rebuild, String)]
rebuildTargets rba rs = foldMap g [ (rba, (RebuildNow ,) <$> patterns rs) ]
    where g (b, ts) = bool mempty ts b
          patterns = thread (mkPattern <$> ["c", "o", "so", "a", "deb"])
          mkPattern ext = ("//*." <> ext :)

cleanConfig :: (MonadIO m) => [String] -> m Pkg
cleanConfig ["clean"] = pure undefined
cleanConfig _         = getConfig Nothing

mkPkg :: Bool -- ^ Force rebuild
      -> Bool -- ^ Run linter
      -> Bool -- ^ Print build profiling information
      -> [IO ()] -- ^ Setup
      -> [String] -- ^ Targets
      -> Maybe String -- ^ Target triple
      -> Int -- ^ Verbosity
      -> IO ()
mkPkg rba lint tim setup rs tgt v = do
    cfg <- cleanConfig rs
    let opt = options rba lint tim v $ pkgToTargets cfg tgt rs
    shake opt $
        mconcat
            [ want (pkgToTargets cfg tgt rs)
            , mkClean
            , pkgToAction setup rs tgt cfg
            ]

mkConfig :: Rules ()
mkConfig =
    (".atspkg" </> "config") %> \out -> do
        need ["atspkg.dhall"]
        x <- liftIO $ input auto "./atspkg.dhall"
        liftIO $ BSL.writeFile out (encode (x :: Pkg))

setTargets :: [String] -> [FilePath] -> Maybe Text -> Rules ()
setTargets rs bins mt = when (null rs) $
    case mt of
        (Just m) -> want . bool bins (manTarget m : bins) =<< pandoc
        Nothing  -> want bins

bits :: Maybe String -> [String] -> Rules ()
bits tgt rs = mconcat $ [ mkManpage, mkInstall tgt, mkConfig ] <>
    sequence [ mkRun, mkTest, mkValgrind ] rs

pkgToTargets :: Pkg -> Maybe String -> [FilePath] -> [FilePath]
pkgToTargets ~Pkg{..} tgt [] = (toTgt tgt . target <$> bin) <> (unpack . libTarget <$> libraries)
pkgToTargets _  _ ts         = ts

noConstr :: ATSConstraint
noConstr = ATSConstraint Nothing Nothing

atslibSetup :: Maybe String -- ^ Optional target triple
            -> String -- ^ Library name
            -> FilePath -- ^ Filepath
            -> IO ()
atslibSetup tgt' lib' p = do
    putStrLn $ "installing " ++ lib' ++ "..."
    subdirs <- (p:) <$> allSubdirs p
    pkgPath <- fromMaybe p <$> findFile subdirs "atspkg.dhall"
    let installDir = takeDirectory pkgPath
    -- removeDirectoryRecursive (installDir </> ".atspkg")
    build' installDir tgt' ["install"]

-- | The directory @~/.atspkg@
pkgHome :: MonadIO m => CCompiler -> m String
pkgHome cc' = liftIO $ (</> (".atspkg" </> ccToDir cc')) <$> getEnv "HOME"

-- | The directory that will be @PATSHOME@.
patsHomeAtsPkg :: MonadIO m => Version -> m String
patsHomeAtsPkg v = fmap (</> show v) (pkgHome (GCC Nothing))

home' :: MonadIO m => Version -- ^ Compiler version
                   -> Version -- ^ Library version
                   -> m String
home' compV libV = do
    h <- patsHomeAtsPkg compV
    pure $ h </> "lib" </> "ats2-postiats-" ++ show libV

-- | This is the @$PATSHOMELOCS@ variable to be passed to the shell.
patsHomeLocsAtsPkg :: Int -- ^ Depth to recurse
                   -> String
patsHomeLocsAtsPkg n = intercalate ":" ((<> ".atspkg/contrib") . ("./" <>) <$> g)
    where g = [ join $ replicate i "../" | i <- [0..n] ]

toTgt :: Maybe String -> Text -> String
toTgt tgt = maybeTgt tgt . unpack
    where maybeTgt (Just t) = (<> ('-' : t))
          maybeTgt Nothing  = id

pkgToAction :: [IO ()] -- ^ Setup actions to be performed
            -> [String] -- ^ Targets
            -> Maybe String -- ^ Optional compiler triple (overrides 'ccompiler')
            -> Pkg -- ^ Package data type
            -> Rules ()
pkgToAction setup rs tgt ~(Pkg bs ts lbs mt _ v v' ds cds bdeps ccLocal cf af as dl slv deb al) =

    unless (rs == ["clean"]) $ do

        let cdps = if f bs || f ts then ("gc", noConstr) : cds else cds where f = any gcBin

        mkUserConfig

        want (unpack . cTarget <$> as)

        newFlag <- do
            exists <- liftIO (doesFileExist flags)
            contents <- if exists then liftIO (BSL.readFile flags) else pure mempty
            pure $ encode tgt /= contents

        -- this is dumb but w/e
        flags %> \out -> do
            if newFlag then alwaysRerun else mempty
            liftIO $ BSL.writeFile out (encode tgt)

        -- TODO depend on tgt somehow?
        specialDeps %> \out -> do
            (_, cfgBin') <- cfgBin
            need [ cfgBin', ".atspkg" </> "config" ]
            v'' <- getVerbosity
            liftIO $ fetchDeps v'' (ccFromString cc') setup (unpack . fst <$> ds) (unpack . fst <$> cdps) (unpack . fst <$> bdeps) cfgBin' atslibSetup False >> writeFile out ""

        let bins = toTgt tgt . target <$> bs
        setTargets rs bins mt

        ph <- home' v' v

        cDepsRules ph >> bits tgt rs

        mapM_ (h ph) lbs

        mapM_ (g ph) (bs ++ ts)

        fold (debRules <$> deb)

    where g ph (Bin s t ls hs' atg gc' extra) =
            atsBin (ATSTarget (unpack <$> cf) (atsToolConfig ph) gc' (unpack <$> ls) [unpack s] hs' (unpackTgt <$> atg) mempty (toTgt tgt t) (deps extra) Executable True)

          h ph (Lib _ s t ls _ hs' lnk atg extra sta) =
            atsBin (ATSTarget (unpack <$> cf) (atsToolConfig ph) False (unpack <$> ls) (unpack <$> s) hs' (unpackTgt <$> atg) (unpackLinks <$> lnk) (unpack t) (deps extra) (k sta) False)

          k False = SharedLibrary
          k True  = StaticLibrary

          atsToolConfig ph = ATSToolConfig ph (patsHomeLocsAtsPkg 5) False (ccFromString cc') (not dl) slv al (unpack <$> af)

          cDepsRules ph = unless (null as) $ do
              let targets = fmap (unpack . cTarget) as
                  sources = fmap (unpack . atsSrc) as
              zipWithM_ (cgen (atsToolConfig ph) [specialDeps, ".atspkg" </> "config"] (fmap (unpack . ats) . atsGen =<< as)) sources targets

          cc' = maybe (unpack ccLocal) (<> "-gcc") tgt
          deps = (flags:) . (specialDeps:) . ((".atspkg" </> "config"):) . fmap unpack

          unpackLinks :: (Text, Text) -> HATSGen
          unpackLinks (t, t') = HATSGen (unpack t) (unpack t')

          unpackTgt :: TargetPair -> ATSGen
          unpackTgt (TargetPair t t' b) = ATSGen (unpack t) (unpack t') b

          specialDeps = ".atspkg" </> "deps" ++ maybe "" ("-" <>) tgt
          flags = ".atspkg" </> "flags"
