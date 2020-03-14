module Development.Shake.ATS.Rules ( atsLex
                                   , cleanATS
                                   , cabalForeign
                                   , getSubdirs
                                   , genATS
                                   , genLinks
                                   ) where

import           Control.Arrow                  (second)
import           Control.Monad
import           Data.Foldable
import           Data.List                      (isSuffixOf)
import           Data.Semigroup                 (Semigroup (..))
import qualified Data.Text.Lazy                 as TL
import           Development.Shake              hiding (doesDirectoryExist)
import           Development.Shake.ATS.Generate
import           Development.Shake.ATS.Type     hiding (ATSTarget (..))
import           Development.Shake.Cabal
import           Development.Shake.FilePath
import           Development.Shake.Version
import           Language.ATS
import           Language.ATS.Generate
import           System.Directory

-- | Given a plain Haskell source file, generate a @.sats@ file containing
-- the equivalent types.
genATS :: FilePath -- ^ Haskell source
       -> FilePattern -- ^ @.sats@ file to generate
       -> Bool -- ^ Whether to call cpphs preprocessor
       -> Rules ()
genATS src' target cpphs' =
    target %> \out -> liftIO $ do
        createDirectoryIfMissing True (takeDirectory out)
        genATSTypes src' out cpphs'

genLinks :: FilePath -> FilePath -> Rules ()
genLinks dats link =
    link %> \out -> traced "genLinks" $ do
        contents <- readFile dats
        let proc = generateLinks contents
        either printErr (flip writeFile out) proc

-- | Get subdirectories recursively.
getSubdirs :: FilePath -> IO [FilePath]
getSubdirs p = do
    ds <- listDirectory p
    case ds of
        [] -> pure []
        xs -> do
            ds' <- filterM doesDirectoryExist ((p </>) <$> xs)
            ss <- traverse getSubdirs ds'
            pure $ ds' <> fold ss

-- | These rules take a @.cabal@ file and the @.o@ file to be produced from
-- them, building the @.o@ file.
cabalForeign :: HsCompiler -> ForeignCabal -> Rules ()
cabalForeign hsc@GHC{} (ForeignCabal cbp' cf' obf') = do

    let cf = TL.unpack cf'
        cbp = maybe cf TL.unpack cbp'
        obf = TL.unpack obf'
        obfDir = takeDirectory (obf -<.> "hs")
        libName = takeBaseName cf

    hcOracle <- hsOracle

    obf %> \out -> do
        let isHaskell path = not (".cabal" `isSuffixOf` path)
        (v, trDeps) <- liftIO $ second (filter isHaskell) <$> getCabalDeps cf

        ghcV' <- quietly ghcVersion
        ghcV <- maybe ghcV' (drop 1) . _suff <$> hcOracle hsc

        need (cf : fmap ((obfDir <> [pathSeparator]) <>) trDeps)
        command_ [Cwd obfDir] "cabal" ["new-build", "all", "-w", "ghc-" ++ ghcV]

        let subdir = takeDirectory cbp
            correctDir = (== "build")
            endsBuild = correctDir . last . splitPath
            pkgDir = subdir </> "dist-newstyle" </> "build" </> platform </> "ghc-" ++ ghcV </> libName ++ "-" ++ prettyShow v

        dir <- filter endsBuild <$> liftIO (getSubdirs pkgDir)
        let obj = head dir </> takeFileName obf
        copyFile' obj out

        let hdr = dropExtension obj ++ "_stub.h"
        copyFile' hdr (takeDirectory out </> takeFileName hdr)
cabalForeign _ _ = error "HsCompiler must be GHC"

-- | Build a @.lats@ file using @atslex@.
atsLex :: FilePath -- ^ Filepath of @.lats@ file
       -> FilePattern -- ^ File pattern for generated output
       -> Rules ()
atsLex latsIn fp =
    fp %> \out -> do
        lats <- liftIO $ readFile latsIn
        (Stdout contents) <- command [Stdin lats] "atslex" []
        liftIO $ writeFile out contents

-- | Clean up after an ATS build.
cleanATS :: Action ()
cleanATS =
    zipWithM_ removeFilesAfter
        [".", ".atspkg", "ats-deps"]
        [["//*_dats.c", "//*_sats.c", "//*_lats.dats", "//tags"], ["//*"], ["//*"]]
