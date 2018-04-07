-- | Module containing functions for working with the
-- [illiterate](http://github.com/vmchale/illiterate) preprocessor for literate
-- programs.
module Development.Shake.Literate ( -- * Action
                                    illiterateA
                                  -- * Rules
                                  , literateHaskell
                                  , literateIdris
                                  , literateAlex
                                  , literateHappy
                                  ) where

import           Development.Shake
import           Development.Shake.FilePath

illiterateA :: FilePath -- ^ Literate source file
            -> FilePath -- ^ Generated source
            -> Action ()
illiterateA inF outF = do
    (Stdout o) <- cmd ["lit", inF]
    liftIO $ writeFile outF o

literateRules :: String -- ^ File extension
              -> Rules ()
literateRules ext = pat %> g
    where pat = "//*." <> ('l' : ext)
          g out = let new = fst (splitExtension out) <> ('.' : ext)
            in illiterateA out new

-- | Rules for building @.lhs@ files.
literateHaskell :: Rules ()
literateHaskell = literateRules "hs"

-- | Rules for building @.lidr@ files.
literateIdris :: Rules ()
literateIdris = literateRules "idr"

-- | Rules for building @.lx@ files
literateAlex :: Rules ()
literateAlex = literateRules "x"

-- | Rules for building @.ly@ files
literateHappy :: Rules ()
literateHappy = literateRules "y"
