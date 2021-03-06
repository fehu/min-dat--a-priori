{-|

Module      : DataAssociation.Itemset.SetImpl
Description : Implementation of Itemset by 'Data.Set'.
License     : MIT
Stability   : development

Contains an instance of @Itemset Data.Set it@.

-}

module DataAssociation.Itemset.SetImpl (

  Itemsets

, Set

) where

import DataAssociation.Definitions
import Data.Set
import Data.Tuple(swap)

-- | Implementation of 'Itemset' by 'Data.Set'.
type Itemsets it = [Set it]

-- | Defined in "DataAssociation.Itemset.SetImpl".
instance (Ord it, Show it) =>
    Itemset Set it where
        setSize = size
        contains = flip isSubsetOf
        containsItem = flip member
        listItems = toList
        newItemset = fromList
        insertItem = insert
        deleteItemAt = deleteAt
        itemsetDiff x y = toList $ difference x y
        splitInit = swap . deleteFindMax

