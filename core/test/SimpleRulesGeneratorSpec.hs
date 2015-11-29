-- |
--
-- Module      : AssocRulesSimpleGeneratorSpec
-- Description : Tests for "DataAssociation.SimpleRulesGenerator".
-- License     :  MIT
-- Stability   : development

module SimpleRulesGeneratorSpec (
  spec
) where

import Test.Hspec

import qualified Data.Set as Set
import Data.List (find)
import Data.Maybe(maybeToList)
import qualified Data.Map as Map
import Data.Function ( on )
import Control.Monad
import Control.Arrow ( (&&&) )

import DataAssociation.Definitions
import DataAssociation.SimpleRulesGenerator.Debug
import TestData


spec :: (Itemset set it, Ord (set it), Ord it) =>
    AssocRulesTestData set it -> Spec
spec ex = describe "DataAssociation.SimpleRulesGenerator" $
          context  "given a precalculated example" $
          describe "must behave as the example given" $ sequence_ $ matchRuleExample ex


matchRuleExample (AssocRulesTestData transactions largeItemsetWithSupport minconf rules) =
    it "should return the same set of rules" (null notInRules && null notInRes) : testRules
    where res = generateAssociationRules' minconf transactions (Map.fromList [largeItemsetWithSupport])
          cres   = Set.fromList res
          crules = Set.fromList rules
          notInRules = filter (`Set.notMember` crules) res
          notInRes   = filter (`Set.notMember` cres) (Set.toList crules)
          testRules = do rule@(AssocRule _ _ conf sup) <- rules
                         let mbResRule = find (rule == ) res
                         let dmain = it "was found" $  mbResRule `shouldBe` Just rule
                         let dext = do resRule <- maybeToList mbResRule
                                       [  it "has same confidence" $ fmap confidence  mbResRule `shouldBe` Just conf
                                        , it "has same support"    $ fmap support mbResRule `shouldBe` Just sup
                                        ]
                         return $ describe (show rule) $ sequence_ $ dmain : dext

