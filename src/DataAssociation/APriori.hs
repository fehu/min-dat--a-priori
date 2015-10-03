{- |

Module      : DataAssociation.APriori
Description : Implements /A-Priory/ Large Itemsets extraction.
License     : MIT
Stability   : development

__A-Priory__ Large 'Itemset's extraction. See <http://rakesh.agrawal-family.com/papers/vldb94apriori.pdf>.

Defines the __APriori__ instance of 'LargeItemsetsExtractor'.
-}

module DataAssociation.APriori where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)

import DataAssociation
import DataAssociation.Definitions
import DataAssociation.Utils
import DataAssociation.Abstract

-----------------------------------------------------------------------------
-- | The __APriori__ instance. Defined in "DataAssociation.APriori". Based on 'apriori'.
instance (Ord (set it), Ord it, Itemset set it) =>
    LargeItemsetsExtractor set it where
        findLargeItemsets minsup rawdata = apriori minsup tr seeds Map.empty
            where tr = (rawdata, length rawdata)
                  itemsCount = sortingGroupBy id length (concatMap listItems rawdata)
                  satisfying = filter f itemsCount
                  f = sufficientSupport minsup (length rawdata) . snd
                  -- itemsets of size 1 with sufficient support
                  seeds = map (newItemset . (:[]) . fst) satisfying

-----------------------------------------------------------------------------
-- | generate Large itemsets with a-priory algorithm. (Figure 1 in the article)
apriori :: (Ord (set it), Ord it, Itemset set it) =>
    MinSupport
    -> ([set it], Int)      -- ^ /transactions/ and their count
    -> [set it]             -- ^ seeds: L_{k-1}
    -> Map (set it) Float   -- ^ __large__ itemsets accumulator
    -> Map (set it) Float   -- ^ __large__ itemsets

apriori mSup@(MinSupport minsup) tr@(transactions, transactionsSize) seeds acc =
    -- error ("cCount = " ++ show cCount)
    if Map.null next then acc
                     else apriori mSup tr (Map.keys next) (Map.union acc next)
    where next = Map.filter (>= minsup) cCount
          cCount     = Map.map (calculateSupport transactionsSize) $
                               countSupported transactions candidates
          candidates = aprioriGen seeds

-----------------------------------------------------------------------------
-- | Apriori Candidate Generation. Generates the L_{k} /candidates/ from L_{k-1} (see 2.1.1 in the article).
--   Consists of `aprioryGenJoin` and `aprioryGenPrune`.
aprioriGen      :: (Itemset set it, Ord it) => [set it] -- ^ L_{k-1}
                                            -> [set it] -- ^ L_{k} /candidates/

-- | Apriori Candidate Generation: Join.
aprioriGenJoin  :: (Itemset set it, Ord it) => [set it] -- ^ L_{k-1}
                                            -> [set it] -- ^ L_{k} /candidates/

-- | Apriori Candidate Generation: Prune.
aprioriGenPrune :: (Itemset set it)         => [set it] -- ^ L_{k-1}
                                            -> [set it] -- ^ L_{k} /candidates/
                                            -> [set it] -- ^ L_{k} /candidates/


aprioriGen = uncurry aprioriGenPrune . preservingArg aprioriGenJoin

aprioriGenJoin seeds = do p <- seeds
                          q <- seeds
                          (diff1, diff2) <- maybeToList $ oneElementDifference p q
                          if diff1 < diff2 then return $ insertItem diff2 p
                                           else []

aprioriGenPrune seeds generated = do g <- generated
                                     [g | all (`elem` seeds) (allSubsetsOneShorter g)]


-----------------------------------------------------------------------------
{- | returns Just ( element contained in the first argument and not the second
                  , element contained in the second argument and not the first)

        if

          (1) the two sets have the same length
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


