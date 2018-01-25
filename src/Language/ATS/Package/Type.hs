{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Type ( Pkg (..)
                                 , Bin (..)
                                 , pkgToAction
                                 , mkPkg
                                 , mkManpage
                                 ) where

import           Control.Composition
import           Control.Monad.IO.Class          (MonadIO)
import           Data.Maybe                      (fromMaybe, isJust)
import           Data.Semigroup                  (Semigroup (..))
import qualified Data.Text.Lazy                  as TL
import           Development.Shake
import           Development.Shake.ATS
import           Development.Shake.FilePath
import           Development.Shake.Man
import           Dhall                           hiding (bool)
import           Language.ATS.Package.Dependency
import           System.Directory                (findExecutable, getCurrentDirectory)

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

pandoc :: (MonadIO m) => m Bool
pandoc = isJust <$> liftIO (findExecutable "pandoc")


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

mkClean :: Rules ()
mkClean =
    "clean" ~> do
    removeFilesAfter "." ["//*.1","//*.c", "tags"]
    removeFilesAfter "target" ["//*"]
    removeFilesAfter ".atspkg" ["//*"]

mkTest :: Rules ()
mkTest =
    "test" ~> do
        config <- getConfig
        let tests = fmap (TL.unpack . target) . test $ config
        need tests
        mapM_ cmd_ tests

pkgToAction :: [String] -> Pkg -> Rules ()
pkgToAction rs (Pkg bs ts mt v v' ds) = do
    unless (rs == ["clean"]) $
        liftIO $ fetchDeps False ds
    action (need ["atspkg.dhall"])
    mapM_ g (bs ++ ts)
    let bins = TL.unpack . target <$> bs
    pa <- pandoc
    when (null rs) $
        case mt of
            (Just m) -> want (bool bins (manTarget m : bins) pa)
            Nothing  -> want bins

    where g (Bin s t ls gc') = atsBin (Version v) (Version v') gc' (TL.unpack <$> ls) (TL.unpack s) (TL.unpack t)

data Bin = Bin { src    :: Text -- ^ Source file (should end with @.dats@)
               , target :: Text -- ^ Binary to be built
               , libs   :: [Text] -- ^ Libraries to link against (e.g. @[ "pthread" ]@)
               , gc     :: Bool } -- ^ Whether to use the garbage collector
         deriving (Show, Eq, Generic, Interpret)

-- data RemotePkg = RemotePkg Pkg Text

data Pkg = Pkg { bin          :: [Bin] -- ^ List of binaries to be built
               , test         :: [Bin] -- ^ List of test suites
               , man          :: Maybe Text -- ^ Optional (markdown) manpages to be converted using @pandoc@.
               , version      :: [Integer] -- ^ Library version
               , compiler     :: [Integer] -- ^ Compiler version
               , dependencies :: [Dependency] -- ^ List of dependencies
               }
         deriving (Show, Eq, Generic, Interpret)
