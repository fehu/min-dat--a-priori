module DataAssociation.Utils (

  preservingArg
, sortingGroupBy
) where

import Control.Arrow ((&&&))
import Data.List
import Data.Function

-- I guess in Haskell should already exist a way to do it,
-- but I don't know it

preservingArg :: (a -> b) -> a -> (a, b)
preservingArg f a = (a, f a)

-- http://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
sortingGroupBy :: (Ord b) => (a -> b) -> ([a] -> c) -> [a] -> [(b, c)]
sortingGroupBy f g = map (f . head &&& g)
                         . groupBy ((==) `on` f)
                         . sortBy (compare `on` f)
