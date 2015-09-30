module DataAssociation (

  Itemset(..)
, ItemsetListing(..)
--, Itemsets(..)

, MinSupport(..)
, MinConfidence(..)

, AssocRule(..)

, mineAssociationRules

) where

import DataAssociation.Definitions
import DataAssociation.Abstract



mineAssociationRules :: ( LargeItemsetsExtractor (set it)
                        , AssociationRulesGenerator set it) =>
    MinSupport -> MinConfidence -> [set it] -> [AssocRule it]

mineAssociationRules ms mc = generateAssociationRules mc . findLargeItemsets ms


