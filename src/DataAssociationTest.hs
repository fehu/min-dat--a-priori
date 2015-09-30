module DataAssociationTest ( run ) where

import WekaData
import DataAssociation
import DataAssociation.Itemset.SetImpl
import DataAssociation.Abstract
import DataAssociation.APriori
import DataAssociation.SimpleRulesGenerator

import qualified Data.Map as Map

filename = "data/supermarket.arff"
--minsup = MinSupport 0.2

run :: IO ()
run = do rawData <- readWekaData filename
         let sparse = wekaData2Sparse rawData
         let itemsets = map newItemset sparse :: Itemsets String

--         let largeItems = findLargeItemsets minsup itemsets
--         print ("largeItems size = " ++ show (Map.size largeItems))
--         sequence_ $ do (item, support) <- Map.assocs largeItems
--                        return $ putStrLn (show item ++ " : " ++ show support)
         let rules = mineAssociationRules (MinSupport 0.2)
                                          (MinConfidence 0.75)
                                          itemsets

         sequence_ $ do rule <- rules
                        return $ print rule
