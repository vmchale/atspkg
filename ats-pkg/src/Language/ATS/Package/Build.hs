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
import qualified Data.Text                       as T
import           Development.Shake               (alwaysRerun, getVerbosity)
import           Development.Shake.ATS
import           Development.Shake.C             (ccFromString)
import           Development.Shake.Check
import           Development.Shake.Clean
import           Development.Shake.Man
import           Distribution.ATS.Version
import           GHC.Conc
import           Language.ATS.Package.Build.C
import           Language.ATS.Package.Compiler
import           Language.ATS.Package.Config
import           Language.ATS.Package.Debian     hiding (libraries, target)
import           Language.ATS.Package.Dependency
import           Language.ATS.Package.Type
import           Quaalude

check :: Maybe String -> Maybe FilePath -> IO Bool
check mStr p = do
    v <- wants mStr p
    doesFileExist =<< getAppUserDataDirectory ("atspkg" </> show v </> "bin" </> "patscc")

wants :: Maybe String -> Maybe FilePath -> IO Version
wants mStr p = compiler <$> getConfig mStr p

-- | Build in current directory or indicated directory
buildAll :: Int
         -> Maybe String
         -> Maybe String
         -> Maybe FilePath
         -> IO ()
buildAll v mStr tgt' p = on (*>) (=<< wants mStr p) fetchDef setupDef
    where fetchDef = fetchCompiler
          setupDef = setupCompiler (toVerbosity v) atslibSetup tgt'

build' :: FilePath -- ^ Directory
       -> Maybe String -- ^ Target triple
       -> Bool -- ^ Debug build?
       -> [String] -- ^ Targets
       -> IO ()
build' dir tgt' dbg rs = withCurrentDirectory dir (mkPkgEmpty mempty)
    where mkPkgEmpty ts = mkPkg Nothing False True False ts rs tgt' dbg 1

-- | Build a set of targets
build :: Int
      -> Bool -- ^ Debug?
      -> [String] -- ^ Targets
      -> IO ()
build v dbg rs = bool (mkPkgEmpty [buildAll v Nothing Nothing Nothing]) (mkPkgEmpty mempty) =<< check Nothing Nothing
    where mkPkgEmpty ts = mkPkg Nothing False True False ts rs Nothing dbg 1

-- TODO clean generated ATS
mkClean :: Rules ()
mkClean = "clean" ~> do
    cleanHaskell
    removeFilesAfter "." ["//*.1", "//*_dats.c", "//*_sats.c", "tags", "//*.a"]
    removeFilesAfter "target" ["//*"]
    removeFilesAfter ".atspkg" ["//*"]
    removeFilesAfter "ats-deps" ["//*"]

-- TODO take more arguments, in particular, include + library dirs
mkInstall :: Maybe String -- ^ Optional target triple
          -> Maybe String -- ^ Optional argument to @atspkg.dhall@
          -> Rules ()
mkInstall tgt mStr =
    "install" ~> do
        config <- getConfig mStr Nothing
        let libs' = fmap (unpack . libTarget) . libraries $ config
            bins = fmap (unpack . target) . bin $ config
            incs = ((fmap unpack . includes) =<<) . libraries $ config
            libDir = maybe mempty (<> [pathSeparator]) tgt
        need (bins <> libs')
        home <- liftIO $ getEnv "HOME"
        atspkgDir <- liftIO $ getAppUserDataDirectory "atspkg"
        let g str = fmap (((home </> str) </>) . takeFileName)
            binDest =  g (".local" </> "bin") bins
            libDest = ((atspkgDir </> libDir </> "lib") </>) . takeFileName <$> libs'
            inclDest = ((atspkgDir </> "include") </>) . takeFileName <$> incs
        zipWithM_ copyFile' (bins ++ libs' ++ incs) (binDest ++ libDest ++ inclDest)
        pa <- pandoc
        case man config of
            Just mt -> when pa $ do
                let mt' = manTarget mt
                    manDest = home </> ".local" </> "share" </> "man" </> "man1" </> takeFileName mt'
                need [mt']
                copyFile' mt' manDest
            Nothing -> pure ()
        co <- compleat
        case completions config of
            Just com -> when co $ do
                let com' = unpack com
                    comDest = home </> ".compleat" </> takeFileName com'
                need [com'] -- FIXME do this all in one step
                copyFile' com' comDest
            Nothing -> pure ()

mkManpage :: Maybe String -> Rules ()
mkManpage mStr = do
    c <- getConfig mStr Nothing
    b <- pandoc
    case man c of
        Just _ -> when b manpages
        _      -> pure ()

parens :: String -> String
parens s = fold [ "(", s, ")" ]

cfgFile :: FilePath
cfgFile = ".atspkg" </> "config"

cfgArgs :: FilePath
cfgArgs = ".atspkg" </> "args"

dhallFile :: FilePath
dhallFile = "atspkg.dhall"

-- FIXME this doesn't rebuild when it should; it should rebuild when
-- @atspkg.dhall@ changes.
getConfig :: MonadIO m => Maybe String -> Maybe FilePath -> m Pkg
getConfig mStr dir' = liftIO $ do
    d <- fromMaybe <$> fmap (</> dhallFile) getCurrentDirectory <*> pure dir'
    b <- not <$> doesFileExist cfgFile
    let go = case mStr of { Just x -> (<> (" " <> parens x)) ; Nothing -> id }
    b' <- shouldWrite mStr cfgArgs
    if b || b'
        then input auto (T.pack (go d))
        else fmap (decode . BSL.fromStrict) . BS.readFile $ cfgFile

manTarget :: Text -> FilePath
manTarget m = unpack m -<.> "1"

mkPhony :: Maybe String -> String -> (String -> String) -> (Pkg -> [Bin]) -> [String] -> Rules ()
mkPhony mStr cmdStr f select rs =
    cmdStr ~> do
        config <- getConfig mStr Nothing
        let runs = bool (filter (/= cmdStr) rs) (fmap (unpack . target) . select $ config) (rs == [cmdStr])
        need runs
        traverse_ cmd_ (f <$> runs)

mkValgrind :: Maybe String -> [String] -> Rules ()
mkValgrind mStr = mkPhony mStr "valgrind" ("valgrind " <>) bin

mkBench :: Maybe String -> [String] -> Rules ()
mkBench mStr = mkPhony mStr "bench" id bench

mkTest :: Maybe String -> [String] -> Rules ()
mkTest mStr = mkPhony mStr "test" id test

mkRun :: Maybe String -> [String] -> Rules ()
mkRun mStr = mkPhony mStr "run" id bin

toVerbosity :: Int -> Verbosity
toVerbosity 0 = Normal
toVerbosity 1 = Loud
toVerbosity 2 = Chatty
toVerbosity 3 = Diagnostic
toVerbosity _ = Diagnostic -- should be a warning

options :: Bool -- ^ Whether to rebuild all targets
        -> Bool -- ^ Whether to run the linter
        -> Bool -- ^ Whether to display profiling information for the build
        -> Int -- ^ Number of CPUs
        -> Int -- ^ Verbosity level
        -> [String] -- ^ A list of targets
        -> ShakeOptions
options rba lint tim cpus v rs = shakeOptions { shakeFiles = ".atspkg"
                                              , shakeThreads = cpus
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

cleanConfig :: (MonadIO m) => Maybe String -> [String] -> m Pkg
cleanConfig _ ["clean"] = pure undefined
cleanConfig mStr _      = getConfig mStr Nothing

mkPkg :: Maybe String -- ^ Optional argument to @atspkg.dhall@
      -> Bool -- ^ Force rebuild
      -> Bool -- ^ Run linter
      -> Bool -- ^ Print build profiling information
      -> [IO ()] -- ^ Setup
      -> [String] -- ^ Targets
      -> Maybe String -- ^ Target triple
      -> Bool -- ^ Debug build?
      -> Int -- ^ Verbosity
      -> IO ()
mkPkg mStr rba lint tim setup rs tgt dbg v = do
    cfg <- cleanConfig mStr rs
    setNumCapabilities =<< getNumProcessors
    cpus <- getNumCapabilities
    let opt = options rba lint tim cpus v $ pkgToTargets cfg tgt rs
    shake opt $
        sequence_
            [ want (pkgToTargets cfg tgt rs)
            , mkClean
            , pkgToAction mStr setup rs tgt dbg cfg
            ]

mkConfig :: Maybe String -> Rules ()
mkConfig mStr = do

    shouldWrite' <- shouldWrite mStr cfgArgs

    cfgArgs %> \out -> do
        alwaysRerun
        exists <- liftIO (doesFileExist out)
        if not exists || shouldWrite'
            then liftIO (BSL.writeFile out (encode mStr))
            else mempty

    cfgFile %> \out -> do
        need [dhallFile, cfgArgs]
        let go = case mStr of { Just x -> (<> (" " <> parens x)) ; Nothing -> id }
        x <- liftIO $ input auto (T.pack (go "./atspkg.dhall"))
        liftIO $ BSL.writeFile out (encode (x :: Pkg))

setTargets :: [String] -> [FilePath] -> Maybe Text -> Rules ()
setTargets rs bins mt = when (null rs) $
    case mt of
        (Just m) -> want . bool bins (manTarget m : bins) =<< pandoc
        Nothing  -> want bins

bits :: Maybe String -> Maybe String -> [String] -> Rules ()
bits mStr tgt rs = sequence_ (sequence [ mkManpage, mkInstall tgt, mkConfig ] mStr) <>
    biaxe [ mkRun, mkTest, mkBench, mkValgrind ] mStr rs

pkgToTargets :: Pkg -> Maybe String -> [FilePath] -> [FilePath]
pkgToTargets ~Pkg{..} tgt [] = (toTgt tgt . target <$> bin) <> (unpack . libTarget <$> libraries) <> (unpack . cTarget <$> atsSource)
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
    pkgPath <- fromMaybe p <$> findFile subdirs dhallFile

    let installDir = takeDirectory pkgPath

    build' installDir tgt' False ["install"]

-- | The directory @~/.atspkg@
pkgHome :: MonadIO m => CCompiler -> m String
pkgHome cc' = liftIO $ getAppUserDataDirectory ("atspkg" </> ccToDir cc')

-- | The directory that will be @PATSHOME@.
patsHomeAtsPkg :: MonadIO m => Version -> m String
patsHomeAtsPkg v = fmap (</> vs) (pkgHome (GCC Nothing Nothing))
    where vs = show v
          -- gmp = if v >= Version [0,3,13] then "gmp-" else ""

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

pkgToAction :: Maybe String -- ^ Optional extra expression to which we should apply @atspkg.dhall@
            -> [IO ()] -- ^ Setup actions to be performed
            -> [String] -- ^ Targets
            -> Maybe String -- ^ Optional compiler triple (overrides 'ccompiler')
            -> Bool -- ^ Debug build?
            -> Pkg -- ^ Package data type
            -> Rules ()
pkgToAction mStr setup rs tgt dbg ~(Pkg bs ts bnchs lbs mt _ v v' ds cds bdeps ccLocal cf af as dl slv deb al) =

    unless (rs == ["clean"]) $ do

        let cdps = if (f bs || f ts || f bnchs) && ("gc" `notElem` (fst <$> cds)) then ("gc", noConstr) : cds else cds where f = any gcBin

        mkUserConfig

        newFlag <- shouldWrite tgt flags

        -- this is dumb but w/e
        flags %> \out -> do
            alwaysRerun
            exists <- liftIO (doesFileExist out)
            liftIO $ if not exists || newFlag
                then BSL.writeFile out (encode tgt)
                else mempty

        -- TODO depend on tgt somehow?
        specialDeps %> \out -> do
            cfgBin' <- cfgBin
            need [ cfgBin', flags, cfgFile ]
            v'' <- getVerbosity
            liftIO $ fetchDeps v'' (ccFromString cc') mStr setup (first unpack <$> ds) (first unpack <$> cdps) (first unpack <$> bdeps) cfgBin' atslibSetup False *> writeFile out ""

        let bins = toTgt tgt . target <$> bs
        setTargets rs bins mt

        ph <- home' v' v

        cDepsRules ph *> bits mStr tgt rs

        traverse_ (h ph) lbs

        traverse_ (g ph) (bs ++ ts ++ bnchs)

        fold (debRules <$> deb)

    where g ph (Bin s t ls hs' atg gc' extra) =
            atsBin (ATSTarget (dbgFlags (unpack <$> cf)) (atsToolConfig ph) gc' (unpack <$> ls) [unpack s] hs' (unpackTgt <$> atg) mempty (toTgt tgt t) (deps extra) Executable (not dbg))

          h ph (Lib _ s t ls _ hs' lnk atg extra sta) =
            atsBin (ATSTarget (dbgFlags (unpack <$> cf)) (atsToolConfig ph) False (unpack <$> ls) (unpack <$> s) hs' (unpackTgt <$> atg) (unpackLinks <$> lnk) (unpack t) (deps extra) (k sta) False)

          dbgFlags = if dbg then ("-g":) else id

          k False = SharedLibrary
          k True  = StaticLibrary

          atsToolConfig ph = ATSToolConfig ph (patsHomeLocsAtsPkg 5) False (ccFromString cc') (not dl) slv al (unpack <$> af)

          cDepsRules ph = unless (null as) $ do
              let targets = fmap (unpack . cTarget) as
                  sources = fmap (unpack . atsSrc) as
              zipWithM_ (cgen (atsToolConfig ph) [specialDeps, cfgFile] (fmap (unpack . ats) . atsGen =<< as)) sources targets

          cc' = maybe (unpack ccLocal) (<> "-gcc") tgt
          deps = (flags:) . (specialDeps:) . (cfgFile:) . fmap unpack

          unpackLinks :: (Text, Text) -> HATSGen
          unpackLinks (t, t') = HATSGen (unpack t) (unpack t')

          unpackTgt :: TargetPair -> ATSGen
          unpackTgt (TargetPair t t' b) = ATSGen (unpack t) (unpack t') b

          specialDeps = ".atspkg" </> "deps" ++ maybe "" ("-" <>) tgt
          flags = ".atspkg" </> "flags"
