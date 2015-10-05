-- |
--
-- Module      :  TestData.TiloBalkeExample
-- Description :  An example from Tilo Balke's presentation.
-- Copyright   :  Wolf-Tilo Balke, Silviu Homoceanu. Institut für Informationssysteme
--                      Technische Universität Braunschweig <http://www.ifis.cs.tu-bs.de>
-- License     :  AllRightsReserved
-- Stability   :  development
--
-- An example from Tilo Balke's presentation.

module TestData.TiloBalkeExample (

  testData

, rulesExample

) where

import Control.Arrow (first, second)
import GHC.Float

import TestData
import DataAssociation.Definitions
import DataAssociation.Utils
import DataAssociation.Itemset.SetImpl

-- | the example
testData :: AprioriTestData Set Int
testData = AprioriTestData (map newItemset testTransactions)
                           (MinSupport 0.5)
                           runs'
    where runs' = do AprioriDebugData seeds joined pruned  <- runs
                     return $ AprioriDebugData (map (first newItemset . second support) seeds)
                                               (map newItemset joined)
                                               (map newItemset pruned)
          transactionsSize = length testTransactions
          support          = (/ int2Float transactionsSize)

-- | test transactions
testTransactions = [ [1, 3, 4]
                   , [2, 3, 5]
                   , [1, 2, 3, 5]
                   , [2, 5]
                   ]

run1 = AprioriDebugData {
  dSeeds = [ ([1], 2)
           , ([2], 3)
           , ([3], 3)
           , ([5], 3)
           ]
, dJoin  = [ [1,2], [1,3], [1,5], [2,3], [2,5], [3,5] ]
, dPrune = [ [1,2], [1,3], [1,5], [2,3], [2,5], [3,5] ]
}

run2 = AprioriDebugData {
  dSeeds = [ ([1,3], 2)
           , ([2,3], 2)
           , ([2,5], 3)
           , ([3,5], 2)
           ]
, dJoin  = [[2, 3, 5]]
, dPrune = [[2, 3, 5]]
}

runs = [run1, run2]


-----------------------------------------------------------------------------

rulesExample :: AssocRulesTestData Set Int
rulesExample = AssocRulesTestData (map newItemset testTransactions)
                                  (newItemset [2, 3, 5], 0.5)
                                  (MinConfidence 0.5)
    [ AssocRulesTestEntry (AssocRule (newItemset [2, 3]) (newItemset [5])) 1                    0.5
    , AssocRulesTestEntry (AssocRule (newItemset [2, 5]) (newItemset [3])) (int2Float 50 / 75)  0.5
    , AssocRulesTestEntry (AssocRule (newItemset [3, 5]) (newItemset [2])) 1                    0.5
    , AssocRulesTestEntry (AssocRule (newItemset [2]) (newItemset [3, 5])) (int2Float 50 / 75)  0.5
    , AssocRulesTestEntry (AssocRule (newItemset [3]) (newItemset [2, 5])) (int2Float 50 / 75)  0.5
    , AssocRulesTestEntry (AssocRule (newItemset [5]) (newItemset [2, 3])) (int2Float 50 / 75)  0.5
    ]

