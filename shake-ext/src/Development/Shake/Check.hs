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
                               ) where

import           Control.Monad.IO.Class
import           Data.Maybe             (isJust)
import           System.Directory       (findExecutable)

-- | Check for the presence of some executable.
checkExecutable :: (MonadIO m) => String -> m Bool
checkExecutable = fmap isJust . liftIO . findExecutable

compleat :: MonadIO m => m Bool
compleat = checkExecutable "compleat"

cabal :: MonadIO m => m Bool
cabal = checkExecutable "cabal"

autoconf :: MonadIO m => m Bool
autoconf = checkExecutable "autoconf"

ghc :: MonadIO m => m Bool
ghc = checkExecutable "ghc"

pandoc :: MonadIO m => m Bool
pandoc = checkExecutable "pandoc"

madlang :: MonadIO m => m Bool
madlang = checkExecutable "madlang"

-- | Check for presence of @pats-filter@.
patsFilter :: MonadIO m => m Bool
patsFilter = checkExecutable "pats-filter"
