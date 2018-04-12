module Main (main) where

import           Criterion.Main
import           Language.C.Dependency

main :: IO ()
main =
    defaultMain [ bgroup "head"
                      [ bench "head" $ whnf head' [(1 :: Integer)..] ]
                ]
