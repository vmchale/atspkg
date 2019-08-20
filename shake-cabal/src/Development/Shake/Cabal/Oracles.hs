{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeFamilies       #-}

-- this is in a separate module because the TypeFamilies extension apparently
-- causes previously valid code to not typecheck... ok
module Development.Shake.Cabal.Oracles ( hsOracle
                                       , cabalOracle
                                       , CabalOracle
                                       , HsCompiler (..)
                                       , CabalVersion (..)
                                       ) where

import           Control.DeepSeq   (NFData)
import           Data.Binary       (Binary)
import           Data.Hashable     (Hashable)
import           Data.Typeable     (Typeable)
import           Development.Shake
import           GHC.Generics      (Generic)

type CabalOracle = CabalVersion -> Action String

-- | Use this for tracking 'HsCompiler'
--
-- @since 0.2.1.0
hsOracle :: (RuleResult q ~ a, q ~ a, ShakeValue q) => Rules (q -> Action a)
hsOracle = addOracle pure

-- | Use this to track the version of cabal globally available
--
-- @since 0.2.1.0
cabalOracle :: Rules CabalOracle
cabalOracle = addOracle $ \CabalVersion -> do
    (Stdout out) <- command [] "cabal" [ "--numeric-version"]
    pure out

data HsCompiler = GHC { _pref :: Maybe String -- ^ Target architecture
                      , _suff :: Maybe String -- ^ Compiler version
                      }
                | GHCJS { _suff :: Maybe String -- ^ Compiler version
                        }
                deriving (Generic, Show, Eq, NFData, Hashable, Binary, Typeable)

data CabalVersion = CabalVersion
    deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult HsCompiler = HsCompiler
type instance RuleResult CabalVersion = String
