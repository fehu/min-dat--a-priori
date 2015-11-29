{- |

Module      : DataAssociation.Utils
Description : Different utilities.
License     : MIT
Stability   : development
-}
module DataAssociation.Utils (

  preservingArg
, sortingGroupBy

, allSubsetsOneShorter

, countSupported
, calculateSupport
, sufficientSupport

) where

import Control.Arrow ((&&&))
import Control.Monad
import Data.List
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Float

import DataAssociation.Definitions

-- I guess in Haskell should already exist a way to do it,
-- but I don't know it

-- | Preserves an argument and returns it, tupled with function's result
--
-- > preservingArg id x == (x, x)
preservingArg :: (a -> b) -> a -> (a, b)
preservingArg f a = (a, f a)

-- | Groups the elements of a list according to /discriminator function/
--    and applies /result function/ to each formed group
--
-- see <http://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby>
sortingGroupBy :: (Ord b) => (a -> b)   -- ^ /discriminator function/
                          -> ([a] -> c) -- ^ /result function/
                          -> [a]        -- ^ a list
                          -> [(b, c)]   -- result of grouping and /result function/ application
sortingGroupBy f g = map (f . head &&& g)
                         . groupBy ((==) `on` f)
                         . sortBy (compare `on` f)


-----------------------------------------------------------------------------
-- | iven an itemset of length l, returns a set of itemsets of length (l - 1)
--                                       that are subsets of the original one.
allSubsetsOneShorter :: (Itemset set it) => set it -> [set it]
allSubsetsOneShorter set = liftM (`deleteItemAt` set) [0 .. setSize set - 1]
-- this equals to:
--      do i <- [0 .. setSize set - 1]
--         return $ deleteItemAt i set


-----------------------------------------------------------------------------
-- | Count the number of occurences of each set in the transactions.
--   An 'occurence' is when the set is a subset of the transaction.
countSupported :: (Ord (set it), Itemset set it) =>
    [set it] -> [set it] -> Map (set it) Int

countSupported transactions sets = Map.fromList cl
    where cl = do set <- sets
                  let cnt tr = if tr `contains` set then 1 else 0
                  let count = foldr (\tr acc -> acc + cnt tr) 0 transactions
                  return (set, count)



calculateSupport :: Int     -- ^ total number of transactions
                 -> Int     -- ^ number of occurences
                 -> Float   -- ^ support
calculateSupport transactionsSize = (/ int2Float transactionsSize) . int2Float


sufficientSupport :: MinSupport
                  -> Int    -- ^ total number of transactions
                  -> Int    -- ^ number of occurences
                  -> Bool   -- ^ has sufficient support?
sufficientSupport (MinSupport minsup) transactionsSize =
    (>= minsup) . calculateSupport transactionsSize



