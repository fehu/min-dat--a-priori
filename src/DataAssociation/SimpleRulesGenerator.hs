-----------------------------------------------------------------------------
-- Generates rules by analysing confidence of the subsets of Large itemsets
--
-- see http://rakesh.agrawal-family.com/papers/vldb94apriori.pdf
-----------------------------------------------------------------------------

module DataAssociation.SimpleRulesGenerator where

import DataAssociation.Definitions
import DataAssociation.Abstract
import DataAssociation.Utils

import Data.Map (Map, member, (!))
import qualified Data.Map as Map
import Control.Arrow( (&&&) )
import GHC.Float

instance (Ord (set it), Ord it, Itemset set it) =>
    AssociationRulesGenerator set it where
        generateAssociationRules minconf transactions largeItemsets =
            error ("subsets size = " ++ show (length subsets))
            where (subsets, _) = subsetsFiltered transactions largeItemsets minconf
            -- TODO


subsetsFiltered :: (Ord (set it), Itemset set it) =>
    [set it] ->
    Map (set it) Float ->
    MinConfidence ->
    ([(set it, [set it])], Map (set it) Float)
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





