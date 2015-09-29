module DataAssociation (

  Itemset(..)
, ItemsetListing(..)
, Itemsets(..)

, MinSupport(..)
, MinConfidence(..)

, AssocRule(..)

, mineAssociationRules

) where

import DataAssociation.Definitions
import DataAssociation.Abstract



mineAssociationRules :: ( LargeItemsetsExtractor set
                        , AssociationRulesGenerator set it) =>
    MinSupport -> MinConfidence -> [set] -> [AssocRule it]

mineAssociationRules ms mc = generateAssociationRules mc . findLargeItemsets ms


