{-# LANGUAGE OverloadedStrings #-}

-- | This module contains scripts to fetch the compiler.
module Language.ATS.Package.Compiler
    ( cleanAll
    , fetchCompiler
    , setupCompiler
    ) where

import           Control.Monad
import           Data.Dependency
import qualified Distribution.ATS   as X
import           System.Directory
import           System.Environment (getEnv)

cleanAll :: IO ()
cleanAll = do
    d <- (++ "/.atspkg") <$> getEnv "HOME"
    b <- doesDirectoryExist d
    when b $ do
        putStrLn "Cleaning everything..."
        removeDirectoryRecursive d

fetchCompiler :: Version -> IO ()
fetchCompiler = X.fetchCompiler Nothing

setupCompiler :: Version -> IO ()
setupCompiler = X.setupCompiler Nothing
