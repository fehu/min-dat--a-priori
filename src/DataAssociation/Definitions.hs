-----------------------------------------------------------------------------
-- rules mining definitions
-----------------------------------------------------------------------------

module DataAssociation.Definitions (

  Itemset(..)
, ItemsetListing(..)
--, Itemsets(..)

, MinSupport(..)
, MinConfidence(..)

, AssocRule(..)

) where


class (Eq set) =>
    Itemset set where
        contains  :: set -> set -> Bool
        setSize   :: set -> Int

class (Itemset (set item)) =>
    ItemsetListing set item where
        listItems   :: set item -> [item]
        -- returns the elements contained in the first argument
        --                                   and not the second
        itemsetDiff :: set item -> set item -> [item]

        insertItem   :: item -> set item -> set item
        deleteItemAt :: Int  -> set item -> set item

        -- creates an itemset from a list of items
        newItemset :: [item] -> set item

newtype MinSupport    = MinSupport Float
newtype MinConfidence = MinConfidence Float

data AssocRule item = AssocRule{ ruleFrom    :: [item]
                               , ruleFollows :: [item]
                               }

--data Itemsets it = forall set. (ItemsetListing set it) =>
--                 Itemsets [set]

