module Data.Dependency.Sort ( sortDeps
                            ) where

import           Data.Dependency.Type
import           Data.Graph
import           Lens.Micro
import           Lens.Micro.Extras

sortDeps :: [Dependency] -> [Dependency]
sortDeps ds = fmap find . topSort $ g
    where (g, find) = asGraph ds
