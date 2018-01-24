{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Type ( Pkg (..)
                                 , Bin (..)
                                 , printConfig
                                 , pkgToAction
                                 , mkPkg
                                 , mkManpage
                                 ) where

import           Control.Composition
import           Control.Monad.IO.Class          (MonadIO)
import           Data.Maybe                      (fromMaybe)
import           Data.Semigroup                  (Semigroup (..))
import qualified Data.Text.Lazy                  as TL
import           Development.Shake
import           Development.Shake.ATS
import           Development.Shake.FilePath
import           Development.Shake.Man
import           Dhall
import           Language.ATS.Package.Dependency

options :: ShakeOptions
options = shakeOptions { shakeFiles = ".atspkg"
                       , shakeThreads = 4
                       , shakeProgress = progressSimple
                       }

mkPkg :: IO ()
mkPkg = shakeArgs options $
    mkTest >>
    mkClean >>
    mkManpage >>
    mkInstall >>
    (pkgToAction =<< getConfig)

mkManpage :: Rules ()
mkManpage = do
    c <- getConfig
    case man c of
        Just _ -> manpages
        _      -> pure ()

getConfig :: MonadIO m => m Pkg
getConfig = liftIO (input auto "./atspkg.dhall")

manTarget :: Text -> FilePath
manTarget m = TL.unpack m -<.> "1"

mkInstall :: Rules ()
mkInstall =
    "install" ~> do
        config <- getConfig
        bins <- fmap (TL.unpack . target) . bin <$> getConfig
        need bins
        home <- fromMaybe "" <$> getEnv "HOME"
        let binDest = fmap (((home <> "/.local/bin/") <>) . takeBaseName) bins
        void $ zipWithM copyFile' bins binDest
        case man config of
            Just mt -> do
                let mt' = manTarget mt
                    manDest = (home <> "/.local/share/man/man1/") <> mt'
                need [mt']
                copyFile' mt' manDest
            Nothing -> pure ()

mkClean :: Rules ()
mkClean =
    "clean" ~> do
    removeFilesAfter "." ["//*.1","//*.c", "tags"]
    removeFilesAfter ".shake" ["//*"]
    removeFilesAfter "target" ["//*"]

mkTest :: Rules ()
mkTest =
    "test" ~> do
        config <- getConfig
        let tests = fmap (TL.unpack . target) . test $ config
        need tests
        mapM_ cmd_ tests

pkgToAction :: Pkg -> Rules ()
pkgToAction (Pkg bs ts mt v v' _) = do
    action (need ["atspkg.dhall"])
    mapM_ g (bs ++ ts)
    let bins = TL.unpack . target <$> bs
    case mt of
        (Just m) -> want (manTarget m : bins)
        Nothing  -> want bins

    where g (Bin s t ls gc') = atsBin (Version v) (Version v') gc' (TL.unpack <$> ls) (TL.unpack s) (TL.unpack t)

data Bin = Bin { src :: Text, target :: Text, libs :: [Text], gc :: Bool }
    deriving (Show, Eq, Generic, Interpret)

data Pkg = Pkg { bin :: [Bin], test :: [Bin], man :: Maybe Text, version :: [Integer], compiler :: [Integer], dependencies :: [Dependency] }
    deriving (Show, Eq, Generic, Interpret)

printConfig :: IO ()
printConfig = do
    x <- input auto "./atspkg.dhall"
    print (x :: Pkg)
