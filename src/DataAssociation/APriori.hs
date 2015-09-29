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

import DataAssociation
import DataAssociation.Abstract


instance (Ord set, Itemset set) =>
    LargeItemsetsExtractor set where
        findLargeItemsets minsup rawdata = undefined

-----------------------------------------------------------------------------
-- generate Large itemsets with a-priory algorithm. (Figure 1 in the article)
apriory :: (Ord set, Itemset set) => MinSupport -> ([set], Int) -> [set] -> [set] -> [set]

apriory minSup@(MinSupport minsup) tr@(transactions, transactionsSize) seeds acc =
    if null next then acc
                 else apriory minSup tr next (acc ++ seeds)
    where next = Map.keys $ Map.filter sufficientSupport cCount
          candidates = aprioryGen seeds
          cCount     = countCandidates transactions candidates
          sufficientSupport = (> minsup) . (/ int2Float transactionsSize) . int2Float

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
aprioryGen      :: (Itemset set) => [set] -> [set]
aprioryGenJoin  :: (Itemset set) => [set] -> [set]
aprioryGenPrune :: (Itemset set) => [set] -> [set]

aprioryGen = aprioryGenPrune . aprioryGenJoin

aprioryGenJoin = undefined
aprioryGenPrune = undefined

