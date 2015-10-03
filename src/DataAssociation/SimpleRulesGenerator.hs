{- |

Module      : DataAssociation.SimpleRulesGenerator
Description : Associasion Rules Generator.
License     : MIT
Stability   : development

Generates rules by analysing confidence of the subsets of __large__ itemsets.

see __1.1__ in <http://rakesh.agrawal-family.com/papers/vldb94apriori.pdf>.

Defines the __SimpleRulesGenerator__ instance of AssociationRulesGenerator.
-}
module DataAssociation.SimpleRulesGenerator where

import DataAssociation.Definitions
import DataAssociation.Abstract
import DataAssociation.Utils

import Data.Map (Map, member, (!))
import qualified Data.Map as Map
import Control.Arrow( (&&&) )
import GHC.Float

-----------------------------------------------------------------------------
-- | The __SimpleRulesGenerator__ instance. Defined in "DataAssociation.SimpleRulesGenerator".
--   Based on 'subsetsFiltered'.
instance (Ord (set it), Ord it, Itemset set it) =>
    AssociationRulesGenerator set it where
        generateAssociationRules minconf transactions largeItemsets =
            do let (parentAndSubsets, _) = subsetsFiltered transactions largeItemsets minconf
               (parent, subsets) <- parentAndSubsets
               subset <- subsets
               return $ AssocRule subset (newItemset $ itemsetDiff parent subset)

-----------------------------------------------------------------------------
-- | Generates subsets of __large__ itemsets with sufficient confidence
subsetsFiltered :: (Ord (set it), Itemset set it) =>
    [set it]                -- ^ /transactions/
    -> Map (set it) Float   -- ^ __large__ itemsets {L_{1}, ..., L_{N}} with corresponding support
    -> MinConfidence        -- ^ minimal confidence for rules
    -> ([(set it, [set it])], Map (set it) Float)   -- ^ ([(/parent/ from L_{?}, its subsets with sufficient confidence)],
                                                    --    support cache)
subsetsFiltered transactions initialParents mS@(MinConfidence minconf) =
    inner (Map.keys initialParents) [] initialParents
    where trSize = length transactions
          inner [] acc supportCache = (acc, supportCache)
          inner parents@(h:_) acc supportCache
            | setSize h == 1 = (acc, supportCache)
            | otherwise = inner next (acc ++ subs) (Map.unions (supportCache:cacheUpd))
                where next = concatMap snd subs
                      subs = map filterSufficientConfidence subs'
                      filterSufficientConfidence (parent, xs) =
                        let children = do (set, sup) <- xs
                                          if (supportCache ! parent) / sup >= minconf
                                           then return set
                                           else []
                        in (parent, children)
                      (subs', cacheUpd) = unzip $ map extract subsEtc
                      extract (large, xs) = ((large, setsC), Map.unions upds)
                        where (setsC, upds) = unzip $ do (set, sup, upd) <- xs
                                                         return ((set, sup), upd)
                      subsEtc = map (id &&& (map supportEtc . allSubsetsOneShorter)) parents
                      supportEtc set = (set, support, cacheUpdate)
                            where (support, cacheUpdate) =
                                    if set `member` supportCache
                                    then (supportCache ! set, Map.empty)
                                    else let sup = Map.map (calculateSupport trSize)
                                                           (countSupported transactions [set])
                                         in (sup ! set, sup)





