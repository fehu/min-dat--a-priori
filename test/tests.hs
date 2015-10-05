module Main (
    main
) where

import Test.Hspec

import AprioriSpec
import AprioriSpec.Data
import AprioriSpec.Data.TiloBalkeExample
import DataAssociation.Itemset.SetImpl

main :: IO ()
main = hspec $ spec testData


