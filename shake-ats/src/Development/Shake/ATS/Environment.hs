module Development.Shake.ATS.Environment ( fixDir
                                         , pkgHome
                                         , patsHome
                                         , ccToDir
                                         , ccToString
                                         , ccFromString
                                         , host
                                         ) where

import           Data.List                  (isPrefixOf, isSuffixOf)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.Lazy             as TL
import           Development.Shake
import           Development.Shake.ATS.Type
import           Development.Shake.FilePath
import           System.Info

host :: String
host = arch ++ withManufacturer os
    where withManufacturer "darwin" = "-apple-" ++ os
          withManufacturer _        = "-unknown-" ++ os

ccToDir :: CCompiler -> String
ccToDir (GCC (Just s) _) = reverse (drop 1 $ reverse s) ++ "/"
ccToDir _                = ""

ccToString :: CCompiler -> String
ccToString Clang          = "clang"
ccToString (Other s)      = s
ccToString (GCC pre suff) = mkQualified pre suff "gcc"
ccToString (GHC pre suff) = mkQualified pre suff "ghc"

ccFromString :: String -> CCompiler
ccFromString "gcc" = GCC Nothing Nothing
ccFromString "clang" = Clang
ccFromString s
    | "gcc" `isSuffixOf` s = GCC (Just (reverse . drop 3 . reverse $ s)) Nothing
    | "gcc" `isPrefixOf` s = GCC Nothing (Just $ drop 3 s)
    | "ghc" `isSuffixOf` s = GHC (Just (reverse . drop 3 . reverse $ s)) Nothing
    | "ghc" `isPrefixOf` s = GHC Nothing (Just $ drop 3 s)
ccFromString _ = GCC Nothing Nothing

mkQualified :: Monoid a => Maybe a -> Maybe a -> a -> a
mkQualified pre suff = h (g <$> [pre, suff])
    where g = maybe id mappend
          h = foldr fmap id

-- | The directory @~/.atspkg@
pkgHome :: CCompiler -> Action String
pkgHome cc' = fromMaybe "/usr/local/" <$> mh
    where mh = fmap (++ ("/.atspkg/" ++ ccToDir cc')) <$> getEnv "HOME"

-- | The directory that will be @PATSHOME@.
patsHome :: Version -> Action String
patsHome v = fmap (++ (show v ++ "/")) (pkgHome (GCC Nothing Nothing))

fixDir :: FilePath -> String -> String
fixDir p =
      TL.unpack
    . TL.replace (TL.pack "./") (TL.pack $ p ++ "/")
    . TL.replace (TL.pack "../") (TL.pack $ joinPath (init $ splitPath p) ++ "/")
    . TL.pack
