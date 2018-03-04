{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.ATS.Package.Generic ( GenericPackage (..)
                                    , InstallDirs (..)
                                    , Package (..)
                                    -- * Functions
                                    , atsInstallDirs
                                    ) where

import           Control.Monad.Reader (ReaderT)
import           Data.Hashable        (Hashable (..))
import           Quaalude

data InstallDirs a = InstallDirs { binDir     :: a -> FilePath
                                 , libDir     :: a -> String -> FilePath
                                 , includeDir :: a -> FilePath
                                 }

atsInstallDirs :: Hashable a => IO (InstallDirs a)
atsInstallDirs = do
    h <- getEnv "HOME"
    pure $ InstallDirs (pure $ h ++ "/.local/bin") (\pkg n -> "/.atspkg/lib/" ++ n ++ "/" ++ hex (hash pkg)) (pure $ h ++ "/.atspkg/include")

newtype Package a b = Package { unPack :: ReaderT (InstallDirs a, [a]) IO b }
    deriving (Functor)
    deriving newtype (Applicative, Monad)

class Hashable a => GenericPackage a where

    binRules :: a -> Package a ()
    libRules :: a -> Package a ()
    includeRules :: a -> Package a ()
