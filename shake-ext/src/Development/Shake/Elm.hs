module Development.Shake.Elm ( elmMake
                             ) where

import           Data.Semigroup
import           Development.Shake

elmMake :: [FilePath] -- ^ Source files
        -> [FilePath] -- ^ Extra source files
        -> FilePattern -- ^ Build output
        -> Rules ()
elmMake sources extras fp =
    fp %> \out -> do
        need (sources <> extras)
        command mempty "elm-make" ("--yes" : "--output" : out : "--warn" : sources)
