-- | The functions in this module get all files in the current directory with
-- some extension.
module Development.Shake.FileDetect
    ( getAts
    , getSats
    , getHats
    , getCats
    , getYml
    , getToml
    , getHs
    , getHappy
    , getAlex
    , getShell
    , getDhall
    , getElm
    , getMadlang
    ) where

import           Control.Monad
import           Data.Semigroup    ((<>))
import           Development.Shake

-- | Get all files ending with @.mad@.
getMadlang :: Action [FilePath]
getMadlang = getAll ["mad"]

getElm :: Action [FilePath]
getElm = getAll ["elm"]

getDhall :: Action [FilePath]
getDhall = getAll ["dhall"]

getYml :: Action [FilePath]
getYml = getAll ["yaml", "yml", "yamllint"]

getToml :: Action [FilePath]
getToml = getAll ["toml"]

-- | Get all haskell source files, including module signatures.
getHs :: [FilePath] -> Action [FilePath]
getHs files = join <$> mapM (`getAllDir` ["hs", "hs-boot", "hsig", "lhs"]) files

getHappy :: Action [FilePath]
getHappy = getAll ["y", "yl"]

getAlex :: Action [FilePath]
getAlex = getAll ["x"]

getShell :: Action [FilePath]
getShell = getAll ["sh"]

get :: String -> Action [FilePath]
get = getAll . pure

getAll :: [String] -> Action [FilePath]
getAll = getAllDir ""

getAllDir :: FilePath -> [String] -> Action [FilePath]
getAllDir dir ss = getDirectoryFiles "" (((dir ++ "//*.") ++) <$> ss)

getCats :: Action [FilePath]
getCats = get "cats"

getSats :: Action [FilePath]
getSats = get "sats"

getDats :: Action [FilePath]
getDats = get "dats"

getHats :: Action [FilePath]
getHats = get "hats"

-- | Get files ending in @.sats@ or @.dats@.
getAts :: Action [FilePath]
getAts = (<>) <$> getDats <*> getSats
