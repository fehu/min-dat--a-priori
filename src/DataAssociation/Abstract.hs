-----------------------------------------------------------------------------
-- rules mining abstractions
-----------------------------------------------------------------------------

module DataAssociation.Abstract (

  LargeItemsetsExtractor(..)
, AssociationRulesGenerator(..)

) where

import DataAssociation.Definitions

class (Itemset set it) =>
    LargeItemsetsExtractor set it where
        findLargeItemsets :: MinSupport -> [set it] -> [set it]

class (Itemset set it) =>
    AssociationRulesGenerator set it where
        generateAssociationRules :: MinConfidence -> [set it] -> [AssocRule it]



