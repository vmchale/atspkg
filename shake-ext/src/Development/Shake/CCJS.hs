{-# LANGUAGE CPP #-}

module Development.Shake.CCJS ( ccjs
                              ) where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory           (createDirectoryIfMissing)

ccjs :: [FilePath] -> FilePattern -> Rules ()
ccjs sources fp =
    fp %> \out -> do
        need sources
        (Stdout sout) <- command mempty "ccjs" (sources <> ["--externs=node"])
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        liftIO $ writeFile out sout
