{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.ATS.Package.Config ( UserConfig (..)
                                   , mkUserConfig
                                   , cfgBin
                                   ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.FileEmbed
import qualified Data.Text            as T
import           Quaalude

data UserConfig = UserConfig { defaultPkgs    :: Text
                             , path           :: Maybe Text
                             , githubUsername :: Text
                             , filterErrors   :: Bool
                             } deriving (Generic, Interpret, Binary)

cfgFile :: String
cfgFile = $(embedStringFile ("dhall" </> "config.dhall"))

defaultFileConfig :: IO ()
defaultFileConfig = do
    dir <- getXdgDirectory XdgConfig "atspkg"
    createDirectoryIfMissing True dir
    writeFile (dir </> "config.dhall") cfgFile

cfgBin :: (MonadIO m) => m FilePath
cfgBin = liftIO $ getAppUserDataDirectory ("atspkg" </> "config")

mkUserConfig :: Rules ()
mkUserConfig = do

    cfgBin' <- cfgBin

    join (unless
        <$> liftIO (doesFileExist cfgBin')
        <*> pure (g cfgBin'))

    where g cfgBin' = do

            cfg <- liftIO (getXdgDirectory XdgConfig ("atspkg" </> "config.dhall"))

            want [cfgBin']

            readUserConfig cfg

            cfgBin' %> \_ -> do
                need [cfg]
                cfgContents <- liftIO $ input auto (T.pack cfg)
                liftIO $ BSL.writeFile cfgBin' (encode (cfgContents :: UserConfig))

readUserConfig :: FilePath -> Rules ()
readUserConfig cfg = do

    want [cfg]

    e <- liftIO $ doesFileExist cfg

    cfg %> \_ -> unless e $
        liftIO defaultFileConfig
