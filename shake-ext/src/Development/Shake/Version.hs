{-# LANGUAGE TemplateHaskell #-}

module Development.Shake.Version ( ghcVersion
                                 , cabalVersion
                                 , commonVersion
                                 , pandocVersion
                                 ) where

import           Development.Shake
import           Development.Shake.TH

ghcVersion :: Action String
ghcVersion = do
    ~(Stdout o) <- command mempty "ghc" ["--numeric-version"]
    pure o

$(mkVersions ["pandoc", "cabal"])
