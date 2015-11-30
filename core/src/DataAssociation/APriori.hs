{- |

Module      : DataAssociation.APriori
Description : Implements /A-Priory/ Large Itemsets extraction.
License     : MIT
Stability   : development

__A-Priory__ Large 'Itemset's extraction. See __2.1__  in <http://rakesh.agrawal-family.com/papers/vldb94apriori.pdf>.

Defines the __APriori__ instance of 'LargeItemsetsExtractor'.
-}

module DataAssociation.APriori where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)

import Control.Arrow

import DataAssociation
import DataAssociation.Definitions
import DataAssociation.Utils
import DataAssociation.Abstract



-----------------------------------------------------------------------------

newtype AprioriCache set it = AprioriCache (Map (set it) Float)

-- | select Large itemsets from given cache
aprioriCached :: (Ord (set it), Ord it, Itemset set it) =>
    AprioriCache set it
    -> MinSupport
    -> Map (set it) Float   -- ^ __large__ itemsets

aprioriCached (AprioriCache cache) (MinSupport minsup) = Map.filter (>= minsup) cache

mkAprioriCache :: (Ord (set it), Ord it, Itemset set it) =>
                  [set it]              -- ^ /transactions/
               -> AprioriCache set it   -- ^ cache

mkAprioriCache = AprioriCache . fst .runApriori (MinSupport 0)

-----------------------------------------------------------------------------
-- | The __APriori__ instance. Defined in "DataAssociation.APriori". Based on 'apriori'.
instance (Ord (set it), Ord it, Itemset set it) =>
    LargeItemsetsExtractor set it where
        findLargeItemsets minsup = fst . runApriori minsup


runApriori minsup@(MinSupport msup) rawdata = apriori' minsup tr seeds Map.empty []
    where tr = (rawdata, length rawdata)
          itemsSup = sortingGroupBy id
                                    (calculateSupport (snd tr) . length)
                                    (concatMap listItems rawdata)
          satisfying = filter ((>= msup) . snd) itemsSup
          -- itemsets of size 1 with sufficient support
          seeds = Map.fromList $ map ((newItemset . (:[]) . fst) &&& snd) satisfying


-----------------------------------------------------------------------------
-- | generate Large itemsets with a-priory algorithm. (Figure 1 in the article)
apriori :: (Ord (set it), Ord it, Itemset set it) =>
    MinSupport
    -> ([set it], Int)      -- ^ /transactions/ and their count
    -> Map (set it) Float   -- ^ seeds L_{k-1} with the corresponding support
    -> Map (set it) Float   -- ^ __large__ itemsets

apriori minsup transactionsWithSize seeds =
    fst $ apriori' minsup transactionsWithSize seeds Map.empty []

-----------------------------------------------------------------------------
-- | inner `apriori` implementation with debugging capacity
apriori' :: (Ord (set it), Ord it, Itemset set it) =>
    MinSupport
    -> ([set it], Int)                                  -- ^ /transactions/ and their count
    -> Map (set it) Float                               -- ^ seeds L_{k-1} with the corresponding support
    -> Map (set it) Float                               -- ^ __large__ itemsets accumulator
    -> [AprioriDebugData set it]                        -- ^ debug data accumulator
    -> (Map (set it) Float, [AprioriDebugData set it ]) -- ^ (__large__ itemsets, debug data)

apriori' mSup@(MinSupport minsup) tr@(transactions, transactionsSize) seeds acc debugAcc =
    if Map.null next then (acc, reverse debugAcc)
                     else apriori' mSup tr next (Map.union acc next) (dd:debugAcc)
    where next = Map.filter (>= minsup) cCount
          cCount     = Map.map (calculateSupport transactionsSize) $
                               countSupported transactions candidates
          (joined, pruned) = aprioriGen' $ Map.keys seeds
          candidates = pruned
          dd = AprioriDebugData (Map.assocs seeds) joined pruned


-- | APriori debug data container
data AprioriDebugData set it = AprioriDebugData {
  dSeeds :: [(set it, Float)] -- ^ debug: apriori seeds
, dJoin  :: [set it]          -- ^ debug: apriori joined
, dPrune :: [set it]          -- ^ debug: apriori pruned
}


-----------------------------------------------------------------------------
-- | Apriori Candidate Generation. Generates the L_{k} /candidates/ from L_{k-1} (see 2.1.1 in the article).
--   Consists of `aprioryGenJoin` and `aprioryGenPrune`.
aprioriGen      :: (Itemset set it, Ord it) => [set it] -- ^ L_{k-1}
                                            -> [set it] -- ^ L_{k} /candidates/

-----------------------------------------------------------------------------
-- | Inner Apriori Candidate Generation with debugging capacity.
aprioriGen'     :: (Itemset set it, Ord it) => [set it]             -- ^ L_{k-1}
                                            -> ([set it], [set it]) -- ^ results of (join, prune)

-- | Apriori Candidate Generation: Join.
aprioriGenJoin  :: (Itemset set it, Ord it) => [set it] -- ^ L_{k-1}
                                            -> [set it] -- ^ L_{k} /candidates/

-- | Apriori Candidate Generation: Prune.
aprioriGenPrune :: (Itemset set it)         => [set it] -- ^ L_{k-1}
                                            -> [set it] -- ^ L_{k} /candidates/
                                            -> [set it] -- ^ L_{k} /candidates/


aprioriGen' sets = (joined, pruned)
    where ((_, joined), pruned) = preservingArg (uncurry aprioriGenPrune)
                                  . preservingArg aprioriGenJoin $ sets

aprioriGen = snd . aprioriGen'

aprioriGenJoin seeds = do p <- seeds
                          q <- seeds
                          (diff1, diff2) <- maybeToList $ lastElementDifference p q -- oneElementDifference -- TODO ASK
                          if diff1 < diff2 then return $ insertItem diff2 p
                                           else []

aprioriGenPrune seeds generated = do g <- generated
                                     [g | all (`elem` seeds) (allSubsetsOneShorter g)]


-----------------------------------------------------------------------------
{- | returns Just ( element contained in the first argument and not the second
                  , element contained in the second argument and not the first)

        if

          1. the two sets have the same length
          2. n-1 elements are the same
          3. one element differs

returns Nothing otherwise
-}
oneElementDifference :: (Itemset set it) => set it -> set it -> Maybe (it, it)
oneElementDifference x y =
    if sameLength && length difference == 1
        then Just (head difference, head difference2)
        else Nothing
    where sameLength  = setSize x == setSize y
          difference  = itemsetDiff x y
          difference2 = itemsetDiff y x


{- | returns Just ( element contained in the first argument and not the second
                  , element contained in the second argument and not the first)

        if

          1. the two sets have the same length
          2. first @n-1@ (sorted) elements of bpth are the same
          3. last elements differ

returns Nothing otherwise
-}
lastElementDifference :: (Itemset set it) => set it -> set it -> Maybe (it, it)
lastElementDifference x y =
    if sameLength && xInit == yInit
        then Just (xLast, yLast)
        else Nothing
    where sameLength = setSize x == setSize y
          (xInit, xLast) = splitInit x
          (yInit, yLast) = splitInit y


