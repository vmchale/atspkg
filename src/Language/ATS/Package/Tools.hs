module Language.ATS.Package.Tools ( getCCompiler
                                  ) where

import           Data.List
import           Development.Shake.ATS

getCCompiler :: String -> CCompiler
getCCompiler "gcc"   = GCC Nothing Nothing
getCCompiler "clang" = Clang
getCCompiler x
    | "gcc" `isPrefixOf` x = GCC (Just $ drop 3 x) Nothing
    | "gcc" `isSuffixOf` x = GCC Nothing (Just . reverse . drop 3 . reverse $ x)
    | otherwise = Other x
