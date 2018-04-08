-- | Module containing functions for working with the
-- [illiterate](http://github.com/vmchale/illiterate) preprocessor for literate
-- programs.
module Development.Shake.Literate ( -- * Action
                                    illiterateA
                                  , unlitA
                                  -- * Rules
                                  , literateHaskell
                                  , literateIdris
                                  , literateAlex
                                  , literateHappy
                                  , literateAgda
                                  ) where

import           Development.Shake
import           Development.Shake.FilePath
import           Language.Preprocessor.Unlit

-- | This uses the [illiterate](https://github.com/vmchale/illiterate)
-- preprocessor.
illiterateA :: FilePath -- ^ Literate source file
            -> FilePath -- ^ Generated source
            -> Action ()
illiterateA inF outF = do
    (Stdout o) <- cmd ["lit", inF]
    liftIO $ writeFile outF o

-- | This uses the 'unlit' function provided by the @cpphs@ package.
unlitA :: FilePath -- ^ Literate source file
       -> FilePath -- ^ Generated source
       -> Action ()
unlitA inF outF = liftIO (g =<< readFile inF)
    where g = writeFile outF . unlit inF

literateRules :: String -- ^ File extension
              -> Rules ()
literateRules ext = pat %> g
    where pat = "//*." <> ('l' : ext)
          g out = let new = fst (splitExtension out) <> ('.' : ext)
            in unlitA out new

-- | Rules for building @.lhs@ files.
literateHaskell :: Rules ()
literateHaskell = literateRules "hs"

-- | Rules for building @.lagda@ files.
literateAgda :: Rules ()
literateAgda = literateRules "agda"

-- | Rules for building @.lidr@ files.
literateIdris :: Rules ()
literateIdris = literateRules "idr"

-- | Rules for building @.lx@ files
literateAlex :: Rules ()
literateAlex = literateRules "x"

-- | Rules for building @.ly@ files
literateHappy :: Rules ()
literateHappy = literateRules "y"
