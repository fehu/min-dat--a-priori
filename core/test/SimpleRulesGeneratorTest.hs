module Main ( main ) where


import DataAssociation
import DataAssociation.SimpleRulesGenerator.Debug
import DataAssociation.Itemset.SetImpl

import TestData
import TestData.TiloBalkeExample

import qualified Data.Map as Map


main :: IO ()
main = do
    let res = generateAssociationRules' (MinConfidence 0.5)
                                        (trTransactions rulesExample)
                                        (Map.fromList [tLargeItemset rulesExample])
    mapM_ print res
