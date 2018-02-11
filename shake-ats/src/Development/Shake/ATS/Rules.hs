module Development.Shake.ATS.Rules ( atsLex
                                   , cleanATS
                                   , cabalExport
                                   , getSubdirs
                                   , genATS
                                   ) where

import           Control.Monad
import           Data.Semigroup             (Semigroup (..))
import qualified Data.Text.Lazy             as TL
import           Development.Shake          hiding (doesDirectoryExist)
import           Development.Shake.ATS.Type hiding (BinaryTarget (..))
import           Development.Shake.Cabal
import           Development.Shake.FilePath
import           Language.ATS.Generate
import           System.Directory           (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)

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

getSubdirs :: FilePath -> IO [FilePath]
getSubdirs p = do
    ds <- listDirectory p
    case ds of
        [] -> pure []
        xs -> do
            ds' <- filterM doesDirectoryExist (((p <> "/") <>) <$> xs)
            ss <- mapM getSubdirs ds'
            pure $ ds' <> join ss

-- TODO - copy the .ghc.environment.* file to the current directory
-- also cabal exports can happen concurrently
cabalExport :: ForeignCabal -> Rules ()
cabalExport (ForeignCabal cf' obf' cbp') = do

    let cf = TL.unpack cf'
        cbp = TL.unpack cbp'
        obf = TL.unpack obf'
        obfDir = takeDirectory (obf -<.> "hs")

    trDeps <- liftIO $ getCabalDeps cf
    obf %> \out -> do

        need (cf : fmap ((obfDir <> "/") <>) trDeps)
        command_ [Cwd obfDir] "cabal" ["new-build"]

        let subdir = takeDirectory cbp ++ "/"
            endsBuild = (== "build") . last . splitPath
        dir <- filter endsBuild <$> liftIO (getSubdirs $ subdir ++ "dist-newstyle/build")
        let obj = head dir ++ "/" ++ takeFileName obf
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
