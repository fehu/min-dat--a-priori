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

module AprioriSpec.Data.TiloBalkeExample (

  testData

) where

import Control.Arrow (first)

import AprioriSpec.Data
import DataAssociation.Definitions
import DataAssociation.Itemset.SetImpl

-- | the example
testData :: AprioriTestData Set Int
testData = AprioriTestData (map newItemset testTransactions)
                           (MinSupport 0.5)
                           (MinConfidence undefined)
                           runs'
    where runs' = do AprioriDebugData seeds joined pruned  <- runs
                     return $ AprioriDebugData (map (first newItemset) seeds)
                                               (map newItemset joined)
                                               (map newItemset pruned)

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
