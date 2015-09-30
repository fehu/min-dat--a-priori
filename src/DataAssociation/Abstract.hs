-----------------------------------------------------------------------------
-- rules mining abstractions
-----------------------------------------------------------------------------

module DataAssociation.Abstract (

  LargeItemsetsExtractor(..)
, AssociationRulesGenerator(..)

) where

import DataAssociation.Definitions

import Data.Map (Map)

class (Itemset set it) =>
    LargeItemsetsExtractor set it where
        findLargeItemsets :: MinSupport -> [set it] -> Map (set it) Float

class (Itemset set it) =>
    AssociationRulesGenerator set it where
        generateAssociationRules :: MinConfidence -> [set it] -> Map (set it) Float -> [AssocRule set it]



