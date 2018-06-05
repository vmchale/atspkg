module Development.Shake.ATS.Environment ( fixDir
                                         , pkgHome
                                         , ccToDir
                                         ) where

import           Control.Monad.IO.Class
import qualified Data.Text.Lazy             as TL
import           Development.Shake.C
import           Development.Shake.FilePath
import           System.Environment         (getEnv)

-- | Given a C compiler, return the appropriate directory for its globally
-- installed artifacts. This is used to keep libraries built for different
-- platforms separate.
ccToDir :: CCompiler -> String
ccToDir (GCC (Just s)) = reverse (drop 1 $ reverse s) ++ "/"
ccToDir _              = ""

-- | The directory @~/.atspkg@
pkgHome :: MonadIO m => CCompiler -> m String
pkgHome cc' = liftIO $ (++ ("/.atspkg/" ++ ccToDir cc')) <$> getEnv "HOME"

fixDir :: FilePath -> String -> String
fixDir p =
      TL.unpack
    . TL.replace (TL.pack "./") (TL.pack $ p ++ "/")
    . TL.replace (TL.pack "../") (TL.pack $ joinPath (init $ splitPath p) ++ "/")
    . TL.replace (TL.pack "$PATSHOMELOCS") (TL.pack ".atspkg/contrib")
    . TL.pack
