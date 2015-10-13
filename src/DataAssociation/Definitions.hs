{- |

Module      : DataAssociation.Definitions
Description : Definitions for rules mining.
License     : MIT
Stability   : development

Definitions for rules mining.

-}
module DataAssociation.Definitions (

  Itemset(..)
, MinSupport(..)
, MinConfidence(..)

, AssocRule(..)

) where


-- | An itemset.
class (Eq (set item), Show (set item), Show item) =>
    Itemset set item where
        -- | a `contains` b
        contains  :: set item -> set item -> Bool
        setSize   :: set item -> Int
        listItems :: set item -> [item]

        -- | returns the elements contained in the first argument
        --                                     and not the second
        itemsetDiff :: set item -> set item -> [item]

        insertItem   :: item -> set item -> set item
        deleteItemAt :: Int  -> set item -> set item

        -- | splits first (n-1) elements and the last
        splitInit :: set item -> (set item, item)

        -- | creates an itemset from a list of items
        newItemset :: [item] -> set item


-- | A container for the /minimum support/ parameter.
newtype MinSupport    = MinSupport Float
-- | A container for the /minimum confidence/ parameter.
newtype MinConfidence = MinConfidence Float


-- | Association Rule
data AssocRule set item = AssocRule{ ruleFrom    :: set item -- ^ implicating itemset
                                   , ruleFollows :: set item -- ^ implication
                                   }
                        deriving (Ord, Eq)

instance (Show item, Itemset set item) =>
    Show (AssocRule set item) where
        show (AssocRule from follows) = show (listItems from) ++ " ==> " ++ show (listItems follows)

