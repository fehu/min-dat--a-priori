-----------------------------------------------------------------------------
-- rules mining definitions
-----------------------------------------------------------------------------

module DataAssociation.Definitions (

  Itemset(..)
, MinSupport(..)
, MinConfidence(..)

, AssocRule(..)

) where


class (Eq (set item)) =>
    Itemset set item where
        -- a `contains` b
        contains  :: set item -> set item -> Bool
        setSize   :: set item -> Int
        listItems :: set item -> [item]

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
