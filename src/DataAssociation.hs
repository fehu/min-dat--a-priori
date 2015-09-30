module DataAssociation (

  Itemset(..)
, MinSupport(..)
, MinConfidence(..)

, AssocRule(..)

, mineAssociationRules

) where

import DataAssociation.Definitions
import DataAssociation.Abstract
import DataAssociation.Utils



mineAssociationRules :: ( LargeItemsetsExtractor set it
                        , AssociationRulesGenerator set it) =>
    MinSupport -> MinConfidence -> [set it] -> [AssocRule it]

mineAssociationRules ms mc = uncurry (generateAssociationRules mc) . preservingArg (findLargeItemsets ms)


