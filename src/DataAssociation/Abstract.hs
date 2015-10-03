{-|

Module      : DataAssociation.Abstract
Description : Rules mining abstractions.
License     : MIT
Stability   : development

Rules mining abstractions.

-}


module DataAssociation.Abstract (

  LargeItemsetsExtractor(..)
, AssociationRulesGenerator(..)

) where

import DataAssociation.Definitions

import Data.Map (Map)



-- | An abstraction for extracting __Large__ 'Itemset's.
class (Itemset set it) =>
    LargeItemsetsExtractor set it where
        findLargeItemsets :: MinSupport         -- ^ the minimal support to consider an itemset __large__
                          -> [set it]           -- ^ input 'Itemset's
                          -> Map (set it) Float -- ^ __large__ itemsets with the corresponding support


-- | An abstraction for generating the association rules from the __large__ 'Itemset's.
class (Itemset set it) =>
    AssociationRulesGenerator set it where
        generateAssociationRules :: MinConfidence       -- ^ the minimal confidence for accepting a rule
                                 -> [set it]            -- ^ the original full list of 'Itemset's
                                 -> Map (set it) Float  -- ^ the __large__ 'Itemset's with the corresponding support
                                 -> [AssocRule set it]  -- ^ the association rules



