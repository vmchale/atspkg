module Development.Shake.Man ( manpages
                             ) where

import           Development.Shake
import           Development.Shake.FilePath

manpages :: Rules ()
manpages =
    "//*.1" %> \out -> do
        let source = out -<.> "md"
        need [ source ]
        cmd ["pandoc", source, "-s", "-t", "man", "-o", out]
