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
import qualified Data.Set as Set
import Control.Arrow( (&&&), second )
import GHC.Float

-----------------------------------------------------------------------------
-- | The __SimpleRulesGenerator__ instance. Defined in "DataAssociation.SimpleRulesGenerator".
--   Based on 'subsetsFiltered'.
instance (Ord (set it), Ord it, Itemset set it) =>
    AssociationRulesGenerator set it where
        generateAssociationRules minconf transactions largeItemsets = rules
            where (rules, _, _) = unzip3 $  generateAssociationRules' minconf transactions largeItemsets


-- | SimpleRulesGenerator debug.
generateAssociationRules' :: (Itemset set it, Ord (set it), Ord it) =>
                             MinConfidence       -- ^ minimal confidence for accepting a rule
                          -> [set it]            -- ^ original full list of 'Itemset's
                          -> Map (set it) Float  -- ^ __large__ 'Itemset's with the corresponding support
                          -> [(AssocRule set it, Float, Float)]  -- ^ @[(association rule, confidence, support)]@

generateAssociationRules' minconf transactions largeItemsets =
    do let (parentAndSubsets, cache) = subsetsFiltered transactions largeItemsets minconf
       (parent, subsets) <- parentAndSubsets
       subset <- subsets
       let spdiff = newItemset $ itemsetDiff parent subset
       let rule = AssocRule subset spdiff
       let sup  = cache ! parent
       let conf = sup / (cache ! subset)
       return (rule, conf, sup)

-----------------------------------------------------------------------------
-- | Generates subsets of __large__ itemsets with sufficient confidence
subsetsFiltered :: (Ord (set it), Itemset set it) =>
    [set it]                -- ^ /transactions/
    -> Map (set it) Float   -- ^ __large__ itemsets {L_{1}, ..., L_{N}} with corresponding support
    -> MinConfidence        -- ^ minimal confidence for rules
    -> ([(set it, [set it])], Map (set it) Float)   -- ^ ([(/parent/ from L_{?}, its subsets with sufficient confidence)],
                                                    --    support cache)
subsetsFiltered transactions parents mS@(MinConfidence minconf) =
    inner (Map.keys parents) [] parents
    where trSize = length transactions
          inner [] acc supportCache           = (map (second Set.toList) acc, supportCache)
          inner (p:parents') acc supportCache =
            inner parents' ((p, Set.map fst subs):acc) (Map.union supportCache cacheUpd)
                where subs = Set.filter filterSufficientConfidence (Set.fromList subs')
                      filterSufficientConfidence (set, sup) = (supportCache ! p) / sup >= minconf
                      (subs', cacheUpd) = extract subsEtc
                      extract xs = (setsC, Map.unions upds)
                        where (setsC, upds) = unzip $ do (set, sup, upd) <- xs
                                                         return ((set, sup), upd)
                      subsEtc = supportEtc $ nonemptySubsets (Set.singleton p) Set.empty
                      nonemptySubsets ps acc =
                            if setSize (Set.findMin ps) == 1
                                then acc
                                else nonemptySubsets subs (Set.union acc subs)
                            where subs = Set.fromList $ concatMap allSubsetsOneShorter (Set.toList ps)
                      supportEtc sets = do
                            set <- Set.toList sets
                            let (support, cacheUpdate) =
                                        if set `member` supportCache
                                        then (supportCache ! set, Map.empty)
                                        else let sup = Map.map (calculateSupport trSize)
                                                               (countSupported transactions [set])
                                             in (sup ! set, sup)
                            return (set, support, cacheUpdate)





