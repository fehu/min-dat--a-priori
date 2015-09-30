-----------------------------------------------------------------------------
-- A-Priory implements Large Itemsets extraction.
--
-- see http://rakesh.agrawal-family.com/papers/vldb94apriori.pdf
-----------------------------------------------------------------------------

module DataAssociation.APriori (

) where

import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Float
import Control.Monad

import DataAssociation
import DataAssociation.Utils
import DataAssociation.Abstract


instance (Ord set, Itemset set) =>
    LargeItemsetsExtractor set where
        findLargeItemsets minsup rawdata = undefined

-----------------------------------------------------------------------------
-- generate Large itemsets with a-priory algorithm. (Figure 1 in the article)
apriory :: (Ord (set it), Ord it, ItemsetListing set it) =>
    MinSupport -> ([set it], Int) -> [set it] -> [set it] -> [set it]

apriory minSup@(MinSupport minsup) tr@(transactions, transactionsSize) seeds acc =
    if null next then acc
                 else apriory minSup tr next (acc ++ next)
    where next = Map.keys $ Map.filter sufficientSupport cCount
          candidates = aprioryGen seeds
          cCount     = countCandidates transactions candidates
          sufficientSupport = (>= minsup) . (/ int2Float transactionsSize) . int2Float

-----------------------------------------------------------------------------
-- count the number of occurences of each candidate in the transactions
-- the 'occurence' is when the candidate is a subset of the transaction
countCandidates :: (Ord set, Itemset set) => [set] -> [set] -> Map set Int

countCandidates transactions candidates = Map.fromList cl
    where cl = do candidate <- candidates
                  let cnt tr = if tr `contains` candidate then 1 else 0
                  let count = foldr (\tr acc -> acc + cnt tr) 0 transactions
                  return (candidate, count)

-----------------------------------------------------------------------------
-- Apriori Candidate Generation. Consists of `join` and `prune`.
-- (2.1.1 in the article)
aprioryGen      :: (ItemsetListing set it, Ord it) => [set it] -> [set it]
aprioryGenJoin  :: (ItemsetListing set it, Ord it) => [set it] -> [set it]
aprioryGenPrune :: (ItemsetListing set it)         => [set it] -> [set it] -> [set it]

--aprioryGen seeds = aprioryGenPrune seeds $ aprioryGenJoin seeds

aprioryGen = uncurry aprioryGenPrune . preservingArg aprioryGenJoin

aprioryGenJoin seeds = do p <- seeds
                          q <- seeds
                          let Just (diff1, diff2) = oneElementDifference p q
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
oneElementDifference :: (ItemsetListing set it) => set it -> set it -> Maybe (it, it)
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
allSubsetsOneShorter :: (ItemsetListing set it) => set it -> [set it]
allSubsetsOneShorter set = liftM (`deleteItemAt` set) [0 .. setSize set]
-- this equals to:
--      do i <- [0 .. setSize set]
--         return $ deleteItemAt i set



