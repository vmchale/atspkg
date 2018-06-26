module Development.Shake.Man ( manpagesA
                             , manpagesR
                             , manpages
                             ) where

import           Development.Shake          hiding ((*>))
import           Development.Shake.FilePath

manpagesA :: FilePath -- ^ Source file. Can be any format accepted by [pandoc](http://hackage.haskell.org/package/pandoc).
          -> FilePath -- ^ Output file.
          -> Action ()
manpagesA source out =
    need [ source ] *>
    cmd ["pandoc", source, "-s", "-t", "man", "-o", out]

manpagesR :: FilePath -- ^ Source file
          -> FilePattern -- ^ Output file pattern
          -> Rules ()
manpagesR source pat =
    pat %> \out -> manpagesA source out

-- | Rules for converting markdown source to manpages.
manpages :: Rules ()
manpages =
    "//*.1" %> \out ->
        let source = out -<.> "md" in
            manpagesA source out
