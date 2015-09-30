module DataAssociationTest ( run ) where

import WekaData
import DataAssociation
import DataAssociation.Itemset.SetImpl
import DataAssociation.Abstract
import DataAssociation.APriori

filename = "data/supermarket.arff"
minsup = MinSupport 0.2

run :: IO ()
run = do rawData <- readWekaData filename
         let sparse = wekaData2Sparse rawData
         let itemsets = map newItemset sparse :: Itemsets String
         let largeItems = findLargeItemsets minsup itemsets
--         let rules = mineAssociationRules (MinSupport 0.75)
--                                          (MinConfidence 0.75)
--                                          itemsets
         print ("largeItems size = " ++ show (length largeItems))
         sequence_ $ do item <- largeItems
                        return $ print item
