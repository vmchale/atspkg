module Development.Shake.Clean ( cleanElm
                               , cleanProducts
                               , cleanHaskell
                               ) where

import           Development.Shake

-- | Clean @elm-stuff@ directory and file extensions typically associated w/ Elm
-- builds.
cleanElm :: Action ()
cleanElm =
    removeFilesAfter "elm-stuff" ["//*"] >>
    removeFilesAfter "." ["//*.elmi"]

-- | Clean generic products (@.o@, @.so@, @.a@).
cleanProducts :: Action ()
cleanProducts = removeFilesAfter "." ["//*.so", "//*.o", "//*.a"]

-- | Clean directories and file extensions typically associated w/ Haskell
-- builds
cleanHaskell :: Action ()
cleanHaskell =
    mapM_ (\p -> removeFilesAfter p ["//*"])
        [ "dist", "dist-newstyle", ".stack-work", ".cabal-sandbox" ] >>
    removeFilesAfter "."
        ["//*.o", "//*.ghc.*", "//*_stub.h", "//*.hi", "//*.dyn_o", "//*.p_o", "//*.dyn_hi", "//*.p_hi", "//*.hc", "//*.haddock", "cabal.sandbox.config"]
