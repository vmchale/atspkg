module Development.Shake.Clean ( cleanElm
                               , cleanProducts
                               , cleanHaskell
                               ) where

import           Development.Shake

cleanElm :: Action ()
cleanElm = removeFilesAfter "elm-stuff" ["//*"]

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
        ["//*.o", "//*.ghc.*", "//*_stub.h", "//*.hi", "//*.dyn_o", "//*.p_o", "//*.dyn_hi", "//*.p_hi", "//*.hc", "cabal.sandbox.config"]
