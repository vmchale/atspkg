module Main where

import           Criterion.Main
import           Language.ATS

main :: IO ()
main =
    defaultMain [ env envFiles $ \ ~(l, m) ->
                  bgroup "format"
                      [ bench "lexATS (large)" $ nf lexATS l
                      , bench "parse (large)" $ nf parse l
                      , bench "fmap printATS . parse (large)" $ nf (fmap printATS . parse) l
                      , bench "lexATS (medium)" $ nf lexATS m
                      , bench "parse (medium)" $ nf parse m
                      , bench "fmap printATS . parse (medium)" $ nf (fmap printATS . parse) m
                      ]
                ]
    where large = readFile "test/data/polyglot.dats"
          medium = readFile "test/data/concurrency.dats"
          envFiles = (,) <$> large <*> medium
