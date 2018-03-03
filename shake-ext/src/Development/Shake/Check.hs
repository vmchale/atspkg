{-# LANGUAGE TemplateHaskell #-}

-- | Functions in this module check for the presence of various build tools.
module Development.Shake.Check ( checkExecutable
                               -- * Helper functions for specific programs
                               , pandoc
                               , autoconf
                               , patsFilter
                               , ghc
                               , compleat
                               , cabal
                               , madlang
                               -- * Types
                               , MBool
                               ) where

import           Control.Monad.IO.Class
import           Development.Shake.TH

$(mkExecChecks ["compleat", "pandoc", "autoconf", "cabal", "ghc", "madlang"])

patsFilter :: (MonadIO m) => m Bool
patsFilter = checkExecutable "pats-filter"
