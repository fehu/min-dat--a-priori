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
import Control.Monad

import DataAssociation.Definitions
import DataAssociation.SimpleRulesGenerator.Debug
import TestData
--import


spec :: (Itemset set it, Ord (set it), Ord it) =>
    AssocRulesTestData set it -> Spec
spec ex = describe "DataAssociation.SimpleRulesGenerator" $
          context  "given a precalculated example" $
          describe "must behave as the example given" $ sequence_ $ matchRuleExample ex


matchRuleExample (AssocRulesTestData transactions largeItemsetWithSupport minconf rules) =
    it "should return the same set of rules" (null notInRules && null notInRes) : testRules
    where res = generateAssociationRules' minconf transactions (Map.fromList [largeItemsetWithSupport])
          cres   = Set.fromList $ map fst' res
          crules = Set.fromList $ map tRule rules
          fst'  (r,_,_) = r
          snd'  (_,c,_) = c
          thrd' (_,_,s) = s
          notInRules = filter ((`Set.notMember` crules) . fst') res
          notInRes   = filter (`Set.notMember` cres) (Set.toList crules)
--          TODO beforeAll printNotIn
--          printNotIn = do putStrLn $ "not in the example: " ++ show notInRules
--                          putStrLn $ "not in the result:  " ++ show notInRes
--                                if null notInRules || null notInRes
--                        then
--                        else mzero
          testRules = do (AssocRulesTestEntry rule conf sup) <- rules
                         let mbResRule = find ((==) rule . fst') res
                         let dmain = it "was found" $ fmap fst'  mbResRule `shouldBe` Just rule
                         let dext = do resRule <- maybeToList mbResRule
                                       [  it "has same confidence" $ fmap snd'  mbResRule `shouldBe` Just conf
                                        , it "has same support"    $ fmap thrd' mbResRule `shouldBe` Just sup
                                        ]
                         return $ describe (show rule) $ sequence_ $ dmain : dext

