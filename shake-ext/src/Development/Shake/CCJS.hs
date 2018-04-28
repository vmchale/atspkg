module Development.Shake.CCJS ( ccjs
                              ) where

import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory           (createDirectoryIfMissing)

ccjs :: [FilePath] -- ^ JavaScript source files
     -> FilePattern -- ^ File pattern for build output
     -> Rules ()
ccjs sources fp =
    fp %> \out -> do
        need sources
        (Stdout sout) <- command mempty "ccjs" (sources ++ ["--externs=node"])
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ writeFile out sout
