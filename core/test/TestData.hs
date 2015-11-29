-- |
--
-- Module      :  AprioriSpec.Data
-- License     :  MIT


module TestData (

  AprioriTestData(..)
, AprioriDebugData(..)

, AssocRulesTestData(..)

) where


import DataAssociation.Definitions
import DataAssociation.APriori.Debug

data AprioriTestData set it = AprioriTestData{
  tTransactions :: [set it]
, tMinsup  :: MinSupport
, tRuns    :: [AprioriDebugData set it]
}


data AssocRulesTestData set it = AssocRulesTestData{
  trTransactions :: [set it]
, tLargeItemset  :: (set it, Float)
, tMinconf       :: MinConfidence
, tRules         :: [AssocRule set it]
}
