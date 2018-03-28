module Development.Shake.Elm ( elmMake
                             ) where

import           Development.Shake

-- | Rules for calling @elm-make@.
elmMake :: [FilePath] -- ^ Elm source files
        -> [FilePath] -- ^ Extra source files to be tracked
        -> FilePattern -- ^ Build output
        -> Rules ()
elmMake sources extras fp =
    fp %> \out -> do
        need (sources <> extras)
        command mempty "elm-make" ("--yes" : "--output" : out : "--warn" : sources)
