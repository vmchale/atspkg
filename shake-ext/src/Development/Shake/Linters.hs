-- findExecutable

module Development.Shake.Linters ( tomlcheck
                                 , yamllint
                                 , hlint
                                 , shellcheck
                                 , ghc
                                 , dhall
                                 , madlang
                                 -- * Formatters
                                 , clangFormat
                                 , atsfmt
                                 , stylishHaskell
                                 -- * File detection
                                 , module Development.Shake.FileDetect
                                 ) where

import           Control.Monad
import           Data.Char                    (isSpace)
import           Development.Shake
import           Development.Shake.FileDetect

-- | Check all @.dhall@ files.
dhall :: Action ()
dhall = mapM_ checkDhall =<< getDhall

checkDhall :: FilePath -> Action ()
checkDhall dh = do
    contents <- liftIO $ readFile dh
    command [Stdin contents] "dhall" []

trim :: String -> String
trim = join fmap f
   where f = reverse . dropWhile isSpace

-- | Check a given formatter is idempotent.
checkIdempotent :: String -> FilePath -> Action ()
checkIdempotent s p = do
    contents <- liftIO $ readFile p
    (Stdout out) <- cmd (s ++ " " ++ p)
    if trim contents == trim out then pure () else error "formatter is not fully applied!"

-- | Check that given files are formatted according to @stylish-haskell@
stylishHaskell :: [FilePath] -> Action ()
stylishHaskell = mapM_ (checkIdempotent "stylish-haskell")

-- | Check that given files are formatted according to @atsfmt@
atsfmt :: [FilePath] -> Action ()
atsfmt = mapM_ (checkIdempotent "atsfmt")

-- | Check that given files are formatted according to @clang-format@
clangFormat :: [FilePath] -> Action ()
clangFormat = mapM_ (checkIdempotent "clang-format")

checkFiles :: String -> [FilePath] -> Action ()
checkFiles str = mapM_ (cmd_ . ((str ++ " ") ++))

-- | Check all @.mad@ files.
madlang :: [FilePath] -> Action ()
madlang = checkFiles "madlang check"

-- | Lint @.sh@ files using
-- [shellcheck](http://hackage.haskell.org/package/ShellCheck).
shellcheck :: [FilePath] -> Action ()
shellcheck = checkFiles "shellcheck"

-- | Check Haskell files using @ghc@.
ghc :: [FilePath] -> Action ()
ghc dirs = checkFiles "ghc -Wall -Werror -Wincomplete-uni-patterns -Wincomplete-record-updates -fno-code" =<< getHs dirs

-- | Check all @.toml@ files using
-- [tomlcheck](http://hackage.haskell.org/package/tomlcheck).
tomlcheck :: Action ()
tomlcheck = checkFiles "tomlcheck --file" =<< getToml

-- | Lint all @.hs@, @.hsig@, and @.hs-boot@ files using
-- [hlint](http://hackage.haskell.org/package/hlint).
hlint :: [FilePath] -> Action ()
hlint dirs = checkFiles "hlint" =<< getHs dirs

-- | Lint all files ending in @yaml@ or @yml@ using
-- [yamllint](https://pypi.python.org/pypi/yamllint).
yamllint :: Action ()
yamllint = checkFiles "yamllint -s" =<< getYml
