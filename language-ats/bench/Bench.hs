module Main where

import           Criterion.Main
import           Language.ATS

main :: IO ()
main =
    defaultMain [ env envFiles $ \ ~(l, m) ->
                  bgroup "format"
                      [ bench "lexATS (large)" $ nf lexATS l
                      , bench "parseATS . lexATS (large)" $ nf parse l
                      , bench "printATS . parseATS . lexATS (large)" $ nf (fmap printATS . parse) l
                      , bench "lexATS (medium)" $ nf lexATS m
                      , bench "parseATS . lexATS (medium)" $ nf parse m
                      , bench "printATS . parseATS . lexATS (medium)" $ nf (fmap printATS . parse) m
                      ]
                ]
    where large = readFile "test/data/polyglot.dats"
          medium = readFile "test/data/concurrency.dats"
          envFiles = (,) <$> large <*> medium
