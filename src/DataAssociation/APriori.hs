-----------------------------------------------------------------------------
-- A-Priory implements Large Itemsets extraction.
--
-- see http://rakesh.agrawal-family.com/papers/vldb94apriori.pdf
-----------------------------------------------------------------------------

module DataAssociation.APriori (

) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import GHC.Float
import Control.Monad

import DataAssociation
import DataAssociation.Utils
import DataAssociation.Abstract


instance (Ord (set it), Ord it, Itemset set it) =>
    LargeItemsetsExtractor set it where
        findLargeItemsets minsup rawdata = apriory minsup tr seeds []
            where tr = (rawdata, length rawdata)
                  itemsCount = sortingGroupBy id length (concatMap listItems rawdata)
                  satisfying = filter f itemsCount
                  f = sufficientSupport minsup (length rawdata) . snd
                  -- itemsets of size 1 with sufficient support
                  seeds = map (newItemset . (:[]) . fst) satisfying

sufficientSupport (MinSupport minsup) transactionsSize =
    (>= minsup) . (/ int2Float transactionsSize) . int2Float

-----------------------------------------------------------------------------
-- generate Large itemsets with a-priory algorithm. (Figure 1 in the article)
apriory :: (Ord (set it), Ord it, Itemset set it) =>
    MinSupport -> ([set it], Int) -> [set it] -> [set it] -> [set it]

apriory minsup tr@(transactions, transactionsSize) seeds acc =
--    error ("cCount = " ++ show cCount)
    if null next then acc
                 else apriory minsup tr next (acc ++ next)
    where next = Map.keys $ Map.filter f cCount
          f    = sufficientSupport minsup transactionsSize
          cCount     = countCandidates transactions candidates
          candidates = aprioryGen seeds

-----------------------------------------------------------------------------
-- count the number of occurences of each candidate in the transactions
-- the 'occurence' is when the candidate is a subset of the transaction
countCandidates :: (Ord (set it), Itemset set it) =>
    [set it] -> [set it] -> Map (set it) Int

countCandidates transactions candidates = Map.fromList cl
    where cl = do candidate <- candidates
                  let cnt tr = if tr `contains` candidate then 1 else 0
                  let count = foldr (\tr acc -> acc + cnt tr) 0 transactions
                  return (candidate, count)

-----------------------------------------------------------------------------
-- Apriori Candidate Generation. Consists of `join` and `prune`.
-- (2.1.1 in the article)
aprioryGen      :: (Itemset set it, Ord it) => [set it] -> [set it]
aprioryGenJoin  :: (Itemset set it, Ord it) => [set it] -> [set it]
aprioryGenPrune :: (Itemset set it)         => [set it] -> [set it] -> [set it]

aprioryGen = uncurry aprioryGenPrune . preservingArg aprioryGenJoin

aprioryGenJoin seeds = do p <- seeds
                          q <- seeds
                          (diff1, diff2) <- maybeToList $ oneElementDifference p q
                          if diff1 < diff2 then return $ insertItem diff2 p
                                           else []

aprioryGenPrune seeds generated = do g <- generated
                                     if all (`elem` seeds) (allSubsetsOneShorter g)
                                        then return g
                                        else mzero


-----------------------------------------------------------------------------
-- returns Just ( element contained in the first argument and not the second
--              , element contained in the second argument and not the first)
--      if 1. the two sets have the same length
--         2. n-1 elements are the same
--         3. one element differs
-- returns Nothing otherwise
oneElementDifference :: (Itemset set it) => set it -> set it -> Maybe (it, it)
oneElementDifference x y =
    if sameLength && length difference == 1
        then Just (head difference, head difference2)
        else Nothing
    where sameLength  = setSize x == setSize y
          difference  = itemsetDiff x y
          difference2 = itemsetDiff y x

-----------------------------------------------------------------------------
-- given an itemset of length l, returns a set of itemsets of length (l - 1)
--                                      that are subsets of the original one
allSubsetsOneShorter :: (Itemset set it) => set it -> [set it]
allSubsetsOneShorter set = liftM (`deleteItemAt` set) [0 .. setSize set - 1]
-- this equals to:
--      do i <- [0 .. setSize set - 1]
--         return $ deleteItemAt i set



