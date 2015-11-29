{- |

Module      : RunDataAssocWekaApriorySimple
Description : Runs data association.
License     : MIT
Stability   : development

Uses "DataAssociation.APriori" and "DataAssociation.SimpleRulesGenerator"
    to generate /data association rules/ from /*arff/ __nominal__ data.

-}

module RunDataAssocWekaApriorySimple ( run ) where

import WekaData
import DataAssociation
import DataAssociation.Itemset.SetImpl
import DataAssociation.APriori
import DataAssociation.SimpleRulesGenerator

type Filename = String


run :: Filename -> MinSupport -> MinConfidence -> IO ()
run filename minsup minconf =
    do rawData <- readWekaData filename
       let sparse = wekaData2Sparse rawData
       let itemsets = map newItemset sparse :: Itemsets String

       let rules = mineAssociationRules minsup minconf itemsets

       sequence_ $ do rule <- rules
                      return $ print rule
