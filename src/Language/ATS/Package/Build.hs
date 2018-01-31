{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Build ( mkPkg
                                  , pkgToAction
                                  , getConfig
                                  ) where

import           Control.Composition
import           Control.Concurrent.ParallelIO.Global
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Binary                          (decode, encode)
import qualified Data.ByteString.Lazy                 as BSL
import           Data.List                            (nub)
import           Data.Maybe                           (fromMaybe)
import           Data.Semigroup                       (Semigroup (..))
import qualified Data.Text.Lazy                       as TL
import           Development.Shake                    hiding (doesFileExist)
import           Development.Shake.ATS
import           Development.Shake.Check
import           Development.Shake.Clean
import           Development.Shake.FilePath
import           Development.Shake.Man
import           Dhall                                hiding (bool)
import           Language.ATS.Package.Dependency
import           Language.ATS.Package.Type
import           System.Directory                     (doesFileExist, getCurrentDirectory)

-- TODO clean generated ATS
mkClean :: Rules ()
mkClean =
    "clean" ~> do
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
                    manDest = (home <> "/.local/share/man/man1/") <> mt'
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

-- getParents :: FilePath -> IO [FilePath]
-- getParents p = do

-- findFile
getConfig :: MonadIO m => Maybe FilePath -> m Pkg
getConfig dir' = liftIO $ do
    d <- fromMaybe <$> fmap (<> "/atspkg.dhall") getCurrentDirectory <*> pure dir'
    b <- not <$> doesFileExist ".atspkg/config"
    if b
        then input auto (TL.pack d)
        else fmap decode . BSL.readFile $ ".atspkg/config"

manTarget :: Text -> FilePath
manTarget m = TL.unpack m -<.> "1"

mkTest :: Rules ()
mkTest =
    "test" ~> do
        config <- getConfig Nothing
        let tests = fmap (TL.unpack . target) . test $ config
        need tests
        mapM_ cmd_ tests

options :: ShakeOptions
options = shakeOptions { shakeFiles = ".atspkg"
                       , shakeThreads = 4
                       , shakeProgress = progressSimple
                       }

-- TODO verbosity & coloring?
mkPkg :: [IO ()] -> [String] -> IO ()
mkPkg setup rs = shake options $
    want rs >>
    mkTest >>
    mkClean >>
    mkManpage >>
    mkInstall >>
    mkConfig >>
    (pkgToAction setup rs =<< getConfig Nothing)

asTuple :: TargetPair -> (Text, Text)
asTuple (TargetPair s t) = (s, t)

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

pkgToAction :: [IO ()] -> [String] -> Pkg -> Rules ()
pkgToAction setup rs (Pkg bs ts mt v v' ds cds cc cf as cdir) = do

    unless (rs == ["clean"]) $
        let cdps = if any gc bs then libcAtomicOps : libcGC (Version [7,6,4]) : cds else cds in
        liftIO $ fetchDeps False setup ds cdps >> stopGlobalPool

    mapM_ g (bs ++ ts)
    let bins = TL.unpack . target <$> bs

    cDeps

    setTargets rs bins mt

    where g (Bin s t ls hs' atg gc') =
            atsBin
                (TL.unpack cc)
                (TL.unpack <$> cf)
                v
                v'
                gc'
                (TL.unpack <$> ls)
                (TL.unpack s)
                (".atspkg/config" : (TL.unpack <$> hs'))
                (both TL.unpack . asTuple <$> atg)
                (TL.unpack t)

          cDeps = unless (null as) $ do
            let cedar = TL.unpack cdir
                atsSourceDirs = nub (takeDirectory . TL.unpack <$> as)
                targets = fmap (((cedar <> "/") <>) . (-<.> "c") . takeBaseName . TL.unpack) as
            want targets
            mapM_ (cgen v v') atsSourceDirs
