module Main (
    main
) where

import Test.Hspec

import qualified AprioriSpec              as ASpec
import qualified SimpleRulesGeneratorSpec as RSpec

import TestData
import TestData.TiloBalkeExample
import DataAssociation.Itemset.SetImpl

main :: IO ()
main = hspec $ do ASpec.spec testData
                  RSpec.spec rulesExample


