{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Build ( mkPkg ) where

import           Control.Composition
import           Control.Concurrent.ParallelIO.Global
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Maybe                           (fromMaybe)
import           Data.Semigroup                       (Semigroup (..))
import qualified Data.Text.Lazy                       as TL
import           Development.Shake
import           Development.Shake.ATS
import           Development.Shake.Check
import           Development.Shake.FilePath
import           Development.Shake.Man
import           Dhall                                hiding (bool)
import           Language.ATS.Package.Dependency
import           Language.ATS.Package.Type
import           System.Directory                     (getCurrentDirectory)

mkClean :: Rules ()
mkClean =
    "clean" ~> do
    removeFilesAfter "." ["//*.1","//*.c", "tags"]
    removeFilesAfter "target" ["//*"]
    removeFilesAfter ".atspkg" ["//*"]

mkInstall :: Rules ()
mkInstall =
    "install" ~> do
        config <- getConfig
        bins <- fmap (TL.unpack . target) . bin <$> getConfig
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
    c <- getConfig
    b <- pandoc
    case man c of
        Just _ -> bool (pure ()) manpages b
        _      -> pure ()

getConfig :: MonadIO m => m Pkg
getConfig = liftIO $ do
    d <- getCurrentDirectory
    input auto (TL.pack d <> "/atspkg.dhall")

manTarget :: Text -> FilePath
manTarget m = TL.unpack m -<.> "1"

mkTest :: Rules ()
mkTest =
    "test" ~> do
        config <- getConfig
        let tests = fmap (TL.unpack . target) . test $ config
        need tests
        mapM_ cmd_ tests

options :: ShakeOptions
options = shakeOptions { shakeFiles = ".atspkg"
                       , shakeThreads = 4
                       , shakeProgress = progressSimple
                       }

-- TODO verbosity & coloring?
mkPkg :: [String] -> IO ()
mkPkg rs = shake options $
    want rs >>
    mkTest >>
    mkClean >>
    mkManpage >>
    mkInstall >>
    (pkgToAction rs =<< getConfig)

-- TODO infer dependencies on gc/atomic gc based on boolean flag.
pkgToAction :: [String] -> Pkg -> Rules ()
pkgToAction rs (Pkg bs ts mt v v' ds cds cc cf as cdir) = do
    unless (rs == ["clean"]) $
        liftIO $ fetchDeps False ds cds >> stopGlobalPool
    action (need ["atspkg.dhall"])
    mapM_ g (bs ++ ts)
    let bins = TL.unpack . target <$> bs
    pa <- pandoc
    cDeps
    when (null rs) $
        case mt of
            (Just m) -> want (bool bins (manTarget m : bins) pa)
            Nothing  -> want bins

    where g (Bin s t ls gc') = atsBin (TL.unpack cc) (TL.unpack <$> cf) (Version v) (Version v') gc' (TL.unpack <$> ls) (TL.unpack s) (TL.unpack t)
          cDeps = unless (null as) $ do
            want $ fmap ((-<.> "c") . takeBaseName . TL.unpack) as
            cgen (Version v) (Version v') (TL.unpack cdir)
