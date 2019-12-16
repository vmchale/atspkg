module Development.Shake.Cabal ( getCabalDeps
                               , getCabalDepsV
                               , getCabalDepsA
                               , shakeVerbosityToCabalVerbosity
                               -- * Oracles
                               , hsOracle
                               , cabalOracle
                               -- * Types
                               , HsCompiler (..)
                               -- * Oracle dummy types
                               , CabalVersion (..)
                               -- * Helper functions
                               , platform
                               , hsCompiler
                               -- * Reëxports from "Distribution.Version"
                               , prettyShow
                               ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Composition
import           Control.Monad
import           Data.Foldable                          (fold, toList)
import           Data.Maybe                             (catMaybes)
import           Development.Shake                      hiding (doesFileExist)
import qualified Development.Shake                      as Shake
import           Development.Shake.Cabal.Oracles
import           Distribution.ModuleName
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parsec
import           Distribution.Pretty
import           Distribution.Types.CondTree
import           Distribution.Types.ForeignLib
import           Distribution.Types.PackageId
import           Distribution.Verbosity                 as Distribution
import           Distribution.Version
import           System.Directory                       (doesFileExist)
import           System.FilePath                        (pathSeparator)
import           System.Info                            (arch, os)

hsCompiler :: HsCompiler -> String
hsCompiler (GHC Nothing Nothing)       = "ghc"
hsCompiler (GHC Nothing (Just v))      = "ghc-" ++ v
hsCompiler (GHC (Just arch') (Just v)) = arch' ++ "-ghc-" ++ v
hsCompiler (GHC (Just arch') Nothing)  = arch' ++ "-ghc"
hsCompiler (GHCJS Nothing)             = "ghcjs"
hsCompiler (GHCJS (Just v))            = "ghcjs-" ++ v

-- | E.g. @x86_64-linux@
platform :: String
platform = arch ++ "-" ++ processOS os
    where processOS "darwin" = "osx"
          processOS x        = x

-- FIXME: should also work with .x, .cpphs, .y, .c2hs files
libraryToFiles :: Library -> [FilePath]
libraryToFiles lib = fold [cs, is, hs]
    where (cs, is) = (cSources &&& includes) $ libBuildInfo lib
          hs = (++ ".hs") . toFilePath <$> explicitLibModules lib

fileHelper :: (a -> [ModuleName]) -> a -> [FilePath]
fileHelper = (fmap ((++ ".hs") . toFilePath) .)

exeToFiles :: Executable -> [FilePath]
exeToFiles = liftA2 (:) modulePath (fileHelper exeModules)

testToFiles :: TestSuite -> [FilePath]
testToFiles = fileHelper testModules

benchToFiles :: Benchmark -> [FilePath]
benchToFiles = fileHelper benchmarkModules

foreignToFiles :: ForeignLib -> [FilePath]
foreignToFiles = fileHelper foreignLibModules

extract :: CondTree a b c -> [c]
extract (CondNode d _ []) = [d]
extract (CondNode d _ bs) = d : (g =<< bs)
    where g (CondBranch _ tb fb) = concat $ catMaybes [Just $ extract tb, extract <$> fb]

-- | Assign each shake @Verbosity@ level to a Cabal @Verbosity@ level.
shakeVerbosityToCabalVerbosity :: Shake.Verbosity -> Distribution.Verbosity
shakeVerbosityToCabalVerbosity Silent     = silent
shakeVerbosityToCabalVerbosity Error      = normal
shakeVerbosityToCabalVerbosity Warn       = normal
shakeVerbosityToCabalVerbosity Info       = verbose
shakeVerbosityToCabalVerbosity Verbose    = verbose
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

        mkHelper f = (toList . fmap snd . f) pkg

        libs = toList (condLibrary pkg)
        exes = mkHelper condExecutables
        subLibs = mkHelper condSubLibraries
        tests = mkHelper condTestSuites
        benches = mkHelper condBenchmarks
        foreigns = mkHelper condForeignLibs

        extractHelper f xs = (f <=< extract) =<< xs

        normalSrc = extractHelper libraryToFiles libs
        exeSrc = extractHelper exeToFiles exes
        subSrc = extractHelper libraryToFiles subLibs
        testSrc = extractHelper testToFiles tests
        benchSrc = extractHelper benchToFiles benches
        foreignSrc = extractHelper foreignToFiles foreigns

        dirHelper f xs = (fmap (++ [pathSeparator]) . hsSourceDirs . f <=< extract) =<< xs

        dir = dirHelper libBuildInfo libs
        exeDir = dirHelper buildInfo exes
        subDirs = dirHelper libBuildInfo subLibs
        testDirs = dirHelper testBuildInfo tests
        benchDirs = dirHelper benchmarkBuildInfo benches
        foreignDirs = dirHelper foreignLibBuildInfo foreigns

        dirgeHelper d = ((++) <$> d <*>)

        dirge = dirgeHelper dir
        dirgeExe = dirgeHelper exeDir
        dirgeSub = dirgeHelper subDirs
        dirgeTest = dirgeHelper testDirs
        dirgeBench = dirgeHelper benchDirs
        dirgeForeign = dirgeHelper foreignDirs

        h = filterM doesFileExist

    norms <- h (dirge normalSrc)
    exeFiles <- h (dirgeExe exeSrc)
    subFiles <- h (dirgeSub subSrc)
    testFiles <- h (dirgeTest testSrc)
    benchFiles <- h (dirgeBench benchSrc)
    foreignFiles <- h (dirgeForeign foreignSrc)

    pure (vers, p : extraSrc ++ norms ++ exeFiles ++ subFiles ++ testFiles ++ benchFiles ++ foreignFiles)
