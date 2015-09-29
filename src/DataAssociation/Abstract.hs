-----------------------------------------------------------------------------
-- rules mining abstractions
-----------------------------------------------------------------------------

module DataAssociation.Abstract (

  LargeItemsetsExtractor(..)
, AssociationRulesGenerator(..)

) where

import DataAssociation.Definitions

class (Itemset set) =>
    LargeItemsetsExtractor set where
        findLargeItemsets :: MinSupport -> [set] -> [set]

class (ItemsetListing set it) =>
    AssociationRulesGenerator set it where
        generateAssociationRules :: MinConfidence -> [set] -> [AssocRule it]



