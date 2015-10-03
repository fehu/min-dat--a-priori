-- |
--
-- Module      :  TestData.TiloBalkeExample
-- Description :  An example from Tilo Balke's presentation.
-- Copyright   :  Wolf-Tilo Balke, Silviu Homoceanu. Institut für Informationssysteme
--                      Technische Universität Braunschweig <http://www.ifis.cs.tu-bs.de>
-- License     :  AllRightsReserved
-- Stability   :  stable
--
-- An example from Tilo Balke's presentation.

module AprioriSpec.Data.TiloBalkeExample (

) where

import Control.Arrow (first)

import AprioriSpec.Data

-- | test transactions
testData = [ [1, 3, 4]
           , [2, 3, 5]
           , [1, 2, 3, 5]
           , [2, 5]
           ]

run1 = AprioriTestRun {
  tSeeds = seeds2str [ ([1], 2)
                     , ([2], 3)
                     , ([3], 3)
                     , ([5], 3)
                     ]
, tJoin  = data2str [ [1,2], [1,3], [1,5], [2,3], [2,5], [3,5] ]
, tPrune = data2str [ [1,2], [1,3], [1,5], [2,3], [2,5], [3,5] ]
}



data2str  = map (map show)
seeds2str = map (first (map show))
--seeds2str = map (\(l,c) -> (map show l, c))
