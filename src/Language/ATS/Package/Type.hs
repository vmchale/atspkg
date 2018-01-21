{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Type ( Pkg (..)
                                 , Bin (..)
                                 , printConfig
                                 , pkgToAction
                                 , mkPkg
                                 ) where

import qualified Data.Text.Lazy        as TL
import           Development.Shake
import           Development.Shake.ATS
import           Dhall

mkPkg :: IO ()
mkPkg = shake shakeOptions
    (pkgToAction =<< liftIO (input auto "./atspkg.dhall"))

pkgToAction :: Pkg -> Rules ()
pkgToAction (Pkg bs ts) = mapM_ g (bs ++ ts) >> want (TL.unpack . target <$> bs)
    where g (Bin s t ls) = atsBin (TL.unpack <$> ls) (TL.unpack s) (TL.unpack t)

data Bin = Bin { src :: Text, target :: Text, libs :: [Text] }
    deriving (Show, Eq, Generic, Interpret)

data Pkg = Pkg { bin :: [Bin], test :: [Bin] }
    deriving (Show, Eq, Generic, Interpret)

printConfig :: IO ()
printConfig = do
    x <- input auto "./atspkg.dhall"
    print (x :: Pkg)
