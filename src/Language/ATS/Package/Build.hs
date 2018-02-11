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

import           Control.Composition
import           Control.Concurrent.ParallelIO.Global
import           Control.Lens
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Binary                          (decode, encode)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BSL
import           Data.List                            (nub)
import           Data.Maybe                           (fromMaybe)
import           Data.Semigroup                       (Semigroup (..))
import qualified Data.Text.Lazy                       as TL
import           Data.Version                         hiding (Version (..))
import           Development.Shake                    hiding (doesFileExist)
import           Development.Shake.ATS
import           Development.Shake.Check
import           Development.Shake.Clean
import           Development.Shake.FilePath
import           Development.Shake.Man
import           Dhall                                hiding (bool, maybe)
import           Language.ATS.Package.Compiler
import           Language.ATS.Package.Config
import           Language.ATS.Package.Dependency
import           Language.ATS.Package.Type            hiding (version)
import           Paths_ats_pkg
import           System.Directory                     (doesFileExist, getCurrentDirectory)
import qualified System.Environment                   as SE

check :: Maybe FilePath -> IO Bool
check p = do
    home <- SE.getEnv "HOME"
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
    removeFilesAfter "." ["//*.1", "//*.c", "tags"]
    removeFilesAfter "target" ["//*"]
    removeFilesAfter ".atspkg" ["//*"]
    removeFilesAfter "ats-deps" ["//*"]

mkInstall :: Rules ()
mkInstall =
    "install" ~> do
        config <- getConfig Nothing
        bins <- fmap (TL.unpack . target) . bin <$> getConfig Nothing
        need bins
        home <- fromMaybe "" <$> getEnv "HOME"
        let binDest = ((home <> "/.local/bin/") <>) . takeBaseName <$> bins
        void $ zipWithM copyFile' bins binDest
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
        then input auto (TL.pack d)
        else fmap (decode . BSL.fromStrict) . BS.readFile $ ".atspkg/config"

manTarget :: Text -> FilePath
manTarget m = TL.unpack m -<.> "1"

mkPhony :: String -> (String -> String) -> (Pkg -> [Bin]) -> [String] -> Rules ()
mkPhony cmdStr f select rs =
    cmdStr ~> do
        config <- getConfig Nothing
        let runs = bool (filter (/= cmdStr) rs) (fmap (TL.unpack . target) . select $ config) (rs == [cmdStr])
        need runs
        mapM_ cmd_ (f <$> runs)

mkValgrind :: [String] -> Rules ()
mkValgrind = mkPhony "valgrind" ("valgrind " <>) bin

mkTest :: [String] -> Rules ()
mkTest = mkPhony "test" id test

mkRun :: [String] -> Rules ()
mkRun = mkPhony "run" id bin

options :: Bool -- ^ Whether to rebuild config
        -> Bool -- ^ Whether to rebuild all targets
        -> Bool -- ^ Whether to run the linter
        -> [String] -- ^ A list of targets
        -> ShakeOptions
options rb rba lint rs = shakeOptions { shakeFiles = ".atspkg"
                          , shakeThreads = 4
                          , shakeLint = bool Nothing (Just LintBasic) lint
                          , shakeVersion = showVersion version
                          , shakeRebuild = foldMap g [ (rb, [(RebuildNow, ".atspkg/config")])
                                                     , (rba, (RebuildNow ,) <$> rs)
                                                     ]
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
mkPkg rb rba lint setup rs tgt _ = do
    cfg <- cleanConfig rs
    let opt = options rb rba lint $ pkgToTargets cfg rs
    shake opt $
        mconcat
            [ want rs
            , mkClean
            , pkgToAction setup rs tgt =<< cleanConfig rs
            ]

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
pkgToTargets ~Pkg{..} [] = TL.unpack . target <$> bin
pkgToTargets _ ts        = ts

-- CROSS-COMPILING atslib:
--
-- 1. use the intmin version
--
-- 2. Then idk?? it's a mess??

pkgToAction :: [IO ()] -- ^ Setup actions to be performed
            -> [String] -- ^ Targets
            -> Maybe String -- ^ Optional compiler triple (overrides 'ccompiler')
            -> Pkg -- ^ Package data type
            -> Rules ()
pkgToAction setup rs tgt ~(Pkg bs ts mt v v' ds cds ccLocal cf as cdir) =

    unless (rs == ["clean"]) $ do

        let cdps = if f bs || f ts then "gc" : cds else cds where f = any gcBin

        mkUserConfig

        "deps" ~> do
            (_, cfgBin') <- cfgBin
            need [ cfgBin' ]
            liftIO $ fetchDeps (ccFromString cc') setup (TL.unpack <$> ds) (TL.unpack <$> cdps) cfgBin' False >> stopGlobalPool

        want [".atspkg/config"]

        let bins = TL.unpack . target <$> bs
        setTargets rs bins mt

        cDepsRules >> bits rs

        mapM_ g (bs ++ ts)

    where g (Bin s t ls hs' atg gc' cSrc) =
            atsBin
                (BinaryTarget (TL.unpack <$> cf) (ATSToolConfig v v' False (ccFromString cc')) gc' (TL.unpack <$> ls) (TL.unpack s) hs' (unpackBoth . asTuple <$> atg) (TL.unpack t) (TL.unpack <$> cSrc) ["deps"] (Binary False))

          cDepsRules = unless (null as) $ do
            let cedar = TL.unpack cdir
                atsSourceDirs = nub (takeDirectory . TL.unpack <$> as)
                targets = fmap (((cedar <> "/") <>) . (-<.> "c") . takeBaseName . TL.unpack) as
            want targets
            hasPF <- patsFilter
            mapM_ (cgen $ ATSToolConfig v v' hasPF (ccFromString cc')) atsSourceDirs

          cc' = maybe (TL.unpack ccLocal) (<> "-gcc") tgt

          unpackBoth :: (Text, Text, Bool) -> (String, String, Bool)
          unpackBoth = over _1 TL.unpack . over _2 TL.unpack
