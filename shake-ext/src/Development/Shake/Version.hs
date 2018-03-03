{-# LANGUAGE TemplateHaskell #-}

module Development.Shake.Version ( ghcVersion
                                 , cabalVersion
                                 , commonVersion
                                 , pandocVersion
                                 ) where

import           Development.Shake.TH

$(mkVersions ["pandoc", "ghc", "cabal"])
