-----------------------------------------------------------------------------
-- rules mining definitions
-----------------------------------------------------------------------------

module DataAssociation.Definitions (

  Itemset(..)
, ItemsetListing(..)
, Itemsets(..)

, MinSupport(..)
, MinConfidence(..)

, AssocRule(..)

) where


class Itemset set where
    contains  :: set -> set -> Bool

class (Itemset set) =>
    ItemsetListing set item where
        listItems :: set -> [item]

newtype MinSupport    = MinSupport Float
newtype MinConfidence = MinConfidence Float

data AssocRule item = AssocRule{ ruleFrom    :: [item]
                               , ruleFollows :: [item]
                               }

data Itemsets it = forall set. (ItemsetListing set it) =>
                 Itemsets [set]

