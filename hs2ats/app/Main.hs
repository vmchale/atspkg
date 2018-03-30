{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Language.ATS.Generate
import           Options.Generic

data Program = Program { src    :: FilePath <?> "Haskell source file"
                       , target :: FilePath <?> "ATS target"
                       , cpphs  :: Bool <?> "Use cpphs as a preprocessor"
                       } deriving (Generic, ParseRecord)

main :: IO ()
main = do
    x <- getRecord "Generate ATS types for Haskell source code" :: IO Program
    genATSTypes (unHelpful . src $ x) (unHelpful . target $ x) (unHelpful . cpphs $ x)
