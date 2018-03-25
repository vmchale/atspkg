module Development.Shake.Cabal ( getCabalDeps
                               , getCabalDepsV
                               , getCabalDepsA
                               , shakeVerbosityToCabalVerbosity
                               -- * Types
                               , HsCompiler (..)
                               -- * Helper functions
                               , platform
                               , hsCompiler
                               -- * Reëxports from "Distribution.Version"
                               , prettyShow
                               ) where

import           Control.Arrow
import           Control.Composition
import           Control.Monad
import           Data.Foldable                          (toList)
import           Data.Maybe                             (catMaybes)
import           Development.Shake                      hiding (doesFileExist)
import qualified Development.Shake                      as Shake
import           Distribution.ModuleName
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parsec
import           Distribution.Pretty
import           Distribution.Types.CondTree
import           Distribution.Types.PackageId
import           Distribution.Verbosity                 as Distribution
import           Distribution.Version
import           System.Directory                       (doesFileExist)
import           System.Info                            (arch, os)

data HsCompiler = GHC { _suff :: Maybe String -- ^ Compiler version
                      }
                | GHCJS { _suff :: Maybe String -- ^ Compiler version
                        }

hsCompiler :: HsCompiler -> String
hsCompiler (GHC Nothing)    = "ghc"
hsCompiler (GHC (Just v))   = "ghc-" <> v
hsCompiler (GHCJS Nothing)  = "ghcjs"
hsCompiler (GHCJS (Just v)) = "ghcjs-" <> v

-- | E.g. @x86_64-linux@
platform :: String
platform = arch ++ "-" ++ os

libraryToFiles :: Library -> [FilePath]
libraryToFiles lib = mconcat [cs, is, hs]
    where (cs, is) = (cSources &&& includes) $ libBuildInfo lib
          hs = (<> ".hs") . toFilePath <$> explicitLibModules lib

extract :: CondTree a b Library -> [Library]
extract (CondNode d _ []) = [d]
extract (CondNode d _ bs) = d : (g =<< bs)
    where g (CondBranch _ tb fb) = join $ catMaybes [Just $ extract tb, extract <$> fb]

-- | Assign each shake @Verbosity@ level to a Cabal @Verbosity@ level.
shakeVerbosityToCabalVerbosity :: Shake.Verbosity -> Distribution.Verbosity
shakeVerbosityToCabalVerbosity Silent     = silent
shakeVerbosityToCabalVerbosity Quiet      = normal
shakeVerbosityToCabalVerbosity Normal     = normal
shakeVerbosityToCabalVerbosity Loud       = verbose
shakeVerbosityToCabalVerbosity Chatty     = verbose
shakeVerbosityToCabalVerbosity Diagnostic = deafening

-- | Get cabal dependencies, respecting verbosity level given to
-- [shake](http://shakebuild.com/).
getCabalDepsA :: FilePath -> Action (Version, [FilePath])
getCabalDepsA = join . (g <$> fmap shakeVerbosityToCabalVerbosity getVerbosity <*>) . pure
    where g = liftIO .* getCabalDepsV

-- | Get library dependencies from a @.cabal@ file. This will only work for
-- @.hs@ files; module signatures are not supported.
getCabalDeps :: FilePath -> IO (Version, [FilePath])
getCabalDeps = getCabalDepsV normal

getCabalDepsV :: Distribution.Verbosity -> FilePath -> IO (Version, [FilePath])
getCabalDepsV v p = do
    pkg <- readGenericPackageDescription v p
    let descr = packageDescription pkg
        extraSrc = extraSrcFiles descr
        vers = pkgVersion (package descr)
        libs = toList (condLibrary pkg)
        normalSrc = (libraryToFiles <=< extract) =<< libs
        dir = (fmap (<> "/") . hsSourceDirs . libBuildInfo <=< extract) =<< libs
        dirge = ((<>) <$> dir <*>)
        h = filterM doesFileExist
    norms <- h (dirge normalSrc)
    pure (vers, extraSrc <> norms)
