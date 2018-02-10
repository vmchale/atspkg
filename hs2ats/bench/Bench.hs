module Main where

import           Criterion.Main
import           Language.ATS.Generate

main :: IO ()
main =
    defaultMain [ env file $ \f ->
                  bgroup "generateATS"
                      [ bench "test/data/HigherOrder.hs" $ nf generateATS f ]
                ]
    where file = readFile "test/data/HigherOrder.hs"
