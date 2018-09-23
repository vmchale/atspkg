{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Function         ((&))
import           Language.ATS.Generate
import           Options.Generic

data Program = Program { src    :: FilePath <?> "Haskell source file"
                       , target :: FilePath <?> "ATS target"
                       , cpphs  :: Bool <?> "Use cpphs as a preprocessor"
                       } deriving (Generic, ParseRecord)

main :: IO ()
main = do
    x <- getRecord "Generate ATS types for Haskell source code" :: IO Program
    let go = (x &) . (unHelpful .)
    genATSTypes (go src) (go target) (go cpphs)
