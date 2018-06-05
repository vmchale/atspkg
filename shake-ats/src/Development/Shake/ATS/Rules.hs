module Development.Shake.ATS.Rules ( atsLex
                                   , cleanATS
                                   , cabalForeign
                                   , getSubdirs
                                   , genATS
                                   , genLinks
                                   ) where

import           Control.Monad
import           Data.Semigroup                 (Semigroup (..))
import qualified Data.Text.Lazy                 as TL
import           Development.Shake              hiding (doesDirectoryExist)
import           Development.Shake.ATS.Generate
import           Development.Shake.ATS.Type     hiding (ATSTarget (..))
import           Development.Shake.C
import           Development.Shake.Cabal        hiding (GHC)
import           Development.Shake.FilePath
import           Development.Shake.Version
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
    link %> \out -> liftIO $ do
        contents <- readFile dats
        let proc = generateLinks contents
        writeFile out (either undefined id proc)

-- | Get subdirectories recursively.
getSubdirs :: FilePath -> IO [FilePath]
getSubdirs p = do
    ds <- listDirectory p
    case ds of
        [] -> pure []
        xs -> do
            ds' <- filterM doesDirectoryExist (((p <> "/") <>) <$> xs)
            ss <- mapM getSubdirs ds'
            pure $ ds' <> join ss

-- | These rules take a @.cabal@ file and the @.o@ file to be produced from
-- them, building the @.o@ file.
cabalForeign :: CCompiler -> ForeignCabal -> Rules ()
cabalForeign (GHC _ suff) (ForeignCabal cbp' cf' obf') = do

    let cf = TL.unpack cf'
        cbp = maybe cf TL.unpack cbp'
        obf = TL.unpack obf'
        obfDir = takeDirectory (obf -<.> "hs")
        libName = takeBaseName cf

    (v, trDeps) <- liftIO $ getCabalDeps cf
    obf %> \out -> do

        ghcV' <- quietly ghcVersion
        let ghcV = maybe ghcV' (drop 1) suff

        need (cf : fmap ((obfDir <> "/") <>) trDeps)
        command_ [Cwd obfDir] "cabal" ["new-build", "all", "-w", "ghc-" ++ ghcV]

        -- TODO move this to the @shake-ext@ package?
        let subdir = takeDirectory cbp ++ "/"
            correctDir = (== "build")
            endsBuild = correctDir . last . splitPath
            pkgDir = subdir ++ "dist-newstyle/build/" ++ platform ++ "/ghc-" ++ ghcV ++ "/" ++ libName ++ "-" ++ prettyShow v ++ "/"

        dir <- filter endsBuild <$> liftIO (getSubdirs pkgDir)
        let obj = head dir ++ "/" ++ takeFileName obf
        liftIO $ copyFile obj out

        let hdr = dropExtension obj ++ "_stub.h"
        liftIO $ copyFile hdr (takeDirectory out ++ "/" ++ takeFileName hdr)
cabalForeign _ _ = mempty

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
        [["//*.c", "//*_lats.dats", "//tags"], ["//*"], ["//*"]]
