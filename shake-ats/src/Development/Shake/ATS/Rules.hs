module Development.Shake.ATS.Rules ( atsLex
                                   , cleanATS
                                   , cabalExport
                                   , getSubdirs
                                   , genATS
                                   , genLinks
                                   ) where

import           Control.Monad
import           Data.List                      (isPrefixOf)
import           Data.Semigroup                 (Semigroup (..))
import qualified Data.Text.Lazy                 as TL
import           Development.Shake              hiding (doesDirectoryExist)
import           Development.Shake.ATS.Generate
import           Development.Shake.ATS.Type     hiding (BinaryTarget (..))
import           Development.Shake.Cabal
import           Development.Shake.FilePath
import           Language.ATS.Generate
import           System.Directory

-- | Given a plain Haskell source file, generate a @.sats@ file containing
-- analogous types.
genATS :: FilePath -- ^ Haskell source
       -> FilePath -- ^ @.sats@ file to generate
       -> Bool -- ^ Whether to call cpphs preprocessor
       -> Rules ()
genATS src target cpphs =
    target %> \out -> liftIO $ do
        createDirectoryIfMissing True (takeDirectory out)
        genATSTypes src out cpphs

genLinks :: FilePath -> FilePath -> Rules ()
genLinks dats link =
    link %> \out -> liftIO $ do
        contents <- readFile dats
        let proc = generateLinks contents
        writeFile out (either undefined id proc)

getSubdirs :: FilePath -> IO [FilePath]
getSubdirs p = do
    ds <- listDirectory p
    case ds of
        [] -> pure []
        xs -> do
            ds' <- filterM doesDirectoryExist (((p <> "/") <>) <$> xs)
            ss <- mapM getSubdirs ds'
            pure $ ds' <> join ss

-- TODO cabal exports can happen concurrently w/ compiler & whatnot
cabalExport :: ForeignCabal -> Rules ()
cabalExport (ForeignCabal cbp' cf' obf') = do

    let cf = TL.unpack cf'
        cbp = maybe cf TL.unpack cbp'
        obf = TL.unpack obf'
        obfDir = takeDirectory (obf -<.> "hs")
        libName = takeBaseName cf

    trDeps <- liftIO $ getCabalDeps cf
    obf %> \out -> do

        need (cf : fmap ((obfDir <> "/") <>) trDeps)
        command_ [Cwd obfDir] "cabal" ["new-build", "all", "-O2"]

        let subdir = takeDirectory cbp ++ "/"
            correctDir = (libName `isPrefixOf`)
            endsBuild = correctDir . last . splitPath
        dir <- filter endsBuild <$> liftIO (getSubdirs $ subdir ++ "dist-newstyle/build")
        let obj = head dir ++ "/opt/build/" ++ takeFileName obf
        liftIO $ copyFile obj out

        let hdr = dropExtension obj ++ "_stub.h"
        liftIO $ copyFile hdr (takeDirectory out ++ "/" ++ takeFileName hdr)

-- | Build a @.lats@ file.
atsLex :: FilePattern -> Rules ()
atsLex fp =
    fp %> \out -> do
        lats <- liftIO $ readFile (out -<.> "lats")
        (Stdout contents) <- command [Stdin lats] "atslex" []
        liftIO $ writeFile out contents

cleanATS :: Rules ()
cleanATS = "clean" ~> do
    removeFilesAfter "." ["//*.c", "//tags"]
    removeFilesAfter ".atspkg" ["//*"]
    removeFilesAfter "ats-deps" ["//*"]
