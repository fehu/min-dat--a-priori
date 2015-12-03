-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.UI.Web.RulesTransfer
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module DataAssociation.Explore.UI.Web.RulesTransfer (

  itemset2JSON
, rule2JSON
, rules2JSON

, GroupedRules
, groupedRules2JSON

) where

import DataAssociation

import Text.JSON

-----------------------------------------------------------------------------

itemset2JSON :: (Show it, Itemset set it) => set it -> JSValue
itemset2JSON = showJSON . map show . listItems

rule2JSON :: (Show it, Itemset set it) => AssocRule set it -> JSValue
rule2JSON (AssocRule left right conf sup) = makeObj [
    ("left",        itemset2JSON left)
  , ("right",       itemset2JSON right)
  , ("support",     showJSON sup)
  , ("confidence",  showJSON conf)
  ]


rules2JSON :: (Show it, Itemset set it) => [AssocRule set it] -> JSValue
rules2JSON = showJSON . map rule2JSON

-----------------------------------------------------------------------------

type GroupedRules set it = [[AssocRule set it]]

groupedRules2JSON :: (Show it, Itemset set it) => GroupedRules set it -> JSValue
groupedRules2JSON = showJSON . map rules2JSON

-----------------------------------------------------------------------------


