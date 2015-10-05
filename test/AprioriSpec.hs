-- |
--
-- Module      : AprioriSpec
-- Description : Tests for "".
-- License     :  MIT
-- Stability   : development

module AprioriSpec ( spec ) where

import Test.Hspec

import Text.Printf

import AprioriSpec.Data
import DataAssociation.Definitions
import DataAssociation.APriori.Debug


spec :: (Itemset set it, Ord (set it), Ord it) =>
    AprioriTestData set it -> Spec

spec ex =
    describe "DataAssociation.APriori" $
    context "given a precalculated example" $
    describe "must behave as the example given" $ sequence_ $ matchWithExample ex



matchWithExample :: (Itemset set it, Ord (set it), Ord it) =>
    AprioriTestData set it -> [Spec]

matchWithExample (AprioriTestData transactions minsup minconf runs) = testNRuns:testRuns
    where (res, ddata) = runApriori minsup transactions
          testNRuns = it "has the same number of runs" $ length runs `shouldBe` length ddata
          testRuns  = do ( AprioriDebugData seeds joined pruned , AprioriDebugData seeds' joined' pruned', i) <- zip3 runs ddata [1..]
                         [
                            it (printf "receives the same seeds  L_{%s}" (show i))     $ seeds  == seeds'
                          , it (printf "produces the same joined L_{%s}" (show $ i+1)) $ joined == joined'
                          , it (printf "produces the same pruned L_{%s}" (show $ i+1)) $ pruned == pruned'
                          ]


