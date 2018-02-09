{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Package.Upgrade ( upgradeAtsPkg
                                    ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char                  (isDigit)
import           Data.List                  (intersperse)
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup
import           Development.Shake.FilePath
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           System.Directory           (createDirectoryIfMissing, renameFile)
import           System.Environment
import           System.Info
import           System.Posix.Files

-- https://github.com/vmchale/atspkg/archive/master.zip
manufacturer :: String
manufacturer = case os of
    "darwin" -> "apple"
    _        -> "unknown"

targetArch :: String
targetArch = g [arch, manufacturer, os]
    where g = mconcat . intersperse "-"

atspkgPath :: IO String
atspkgPath = do
    home <- fromMaybe "." <$> lookupEnv "HOME"
    pure $ home <> "/.local/bin/atspkg"

-- TODO install `pi`?
upgradeAtsPkg :: IO ()
upgradeAtsPkg = do

    putStrLn "Finding latest release..."
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest "https://github.com/vmchale/atspkg/releases/latest"
    response <- responseBody <$> httpLbs (initialRequest { method = "GET", redirectCount = 0 }) manager

    putStrLn "Downloading latest release..."
    let strVersion = BSL.takeWhile (/='"') . BSL.dropWhile (not . isDigit) . BSL.dropWhile (/='"') $ response
        binRequest = "https://github.com/vmchale/atspkg/releases/download/" <> BSL.unpack strVersion <> "/atspkg-" <> targetArch
    followupRequest <- parseRequest binRequest
    binBytes <- responseBody <$> httpLbs (followupRequest { method = "GET" }) manager

    atsPath <- atspkgPath
    createDirectoryIfMissing True (takeDirectory atsPath)
    BSL.writeFile (atsPath ++ "-new") binBytes
    renameFile (atsPath ++ "-new") atsPath
    setFileMode atsPath ownerModes
