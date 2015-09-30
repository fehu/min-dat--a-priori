-----------------------------------------------------------------------------
-- Implementation of Itemset by Data.Set
-----------------------------------------------------------------------------

module DataAssociation.Itemset.SetImpl (

  Itemsets

) where

import DataAssociation.Definitions

import Data.Set

type Itemsets it = [Set it]

instance (Ord it, Show it) =>
    Itemset Set it where
        setSize = size
        contains = flip isSubsetOf
        listItems = toList
        newItemset = fromList
        insertItem = insert
        deleteItemAt = deleteAt
        itemsetDiff x y = toList $ difference x y

