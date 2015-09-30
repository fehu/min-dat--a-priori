-----------------------------------------------------------------------------
-- Implementation of Itemset by Data.Set
-----------------------------------------------------------------------------

module DataAssociation.Itemset.SetImpl where

import DataAssociation.Definitions

import Data.Set


instance (Ord it) =>
    Itemset Set it where
        setSize = size
        contains = isSubsetOf
        listItems = toList
        newItemset = fromList
        insertItem = insert
        deleteItemAt = deleteAt
        itemsetDiff x y = toList $ difference x y

