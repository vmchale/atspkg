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

defaultFileConfig :: FilePath -> IO ()
defaultFileConfig p = do
    let dir = p </> ".config" </> "atspkg"
    createDirectoryIfMissing True dir
    writeFile (dir </> "config.dhall") cfgFile

cfgBin :: (MonadIO m) => m (FilePath, FilePath)
cfgBin = liftIO io
    where io = (id &&& (</> (".atspkg" </> "config"))) <$> getEnv "HOME"

mkUserConfig :: Rules ()
mkUserConfig = do

    (h, cfgBin') <- cfgBin

    join (unless
        <$> liftIO (doesFileExist cfgBin')
        <*> pure (g h cfgBin'))

    where g h cfgBin' = do

            let cfg = h </> ".config" </> "atspkg" </> "config.dhall"

            want [cfgBin']

            readUserConfig h cfg

            cfgBin' %> \_ -> do
                need [cfg]
                cfgContents <- liftIO $ input auto (T.pack cfg)
                liftIO $ BSL.writeFile cfgBin' (encode (cfgContents :: UserConfig))

readUserConfig :: FilePath -> FilePath -> Rules ()
readUserConfig h cfg = do

    want [cfg]

    e <- liftIO $ doesFileExist cfg

    cfg %> \_ -> unless e $
        liftIO (defaultFileConfig h)
