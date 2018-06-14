module Development.Shake.Version ( ghcVersion
                                 , cabalVersion
                                 , commonVersion
                                 , pandocVersion
                                 ) where

import           Development.Shake

-- | Attempt to get version information from a given exectuable.
commonVersion :: String -- ^ Executable name
              -> Action String
commonVersion prog = do
    ~(Stdout out) <- command [] prog ["--version"]
    pure . last . words . head . lines $ out

ghcVersion :: Action String
ghcVersion = do
    ~(Stdout o) <- command [] "ghc" ["--numeric-version"]
    pure (head (lines o))

cabalVersion :: Action String
cabalVersion = commonVersion "cabal"

pandocVersion :: Action String
pandocVersion = commonVersion "pandoc"
