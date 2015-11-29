{-# OPTIONS_HADDOCK ignore-exports, show-extensions #-}

{-|

Module      : DataAssociation
Description : Association Rules Mining. Root module.
License     : MIT
Stability   : development

The root module of Association Rules Mining.

-}
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

{- |
 Searches for the 'AssocRule's in the given itemsets.
 Instances of 'LargeItemsetsExtractor' and 'AssociationRulesGenerator' must be in the scope.
 Is composed of /findLargeItemsets/ and /generateAssociationRules/, defined in "DataAssociation.Abstract".
-}
mineAssociationRules :: ( LargeItemsetsExtractor set it
                        , AssociationRulesGenerator set it) =>
    MinSupport -> MinConfidence -> [set it] -> [AssocRule set it]

mineAssociationRules ms mc = uncurry (generateAssociationRules mc) . preservingArg (findLargeItemsets ms)


