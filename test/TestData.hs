-- |
--
-- Module      :  AprioriSpec.Data
-- License     :  MIT


module TestData (

  AprioriTestData(..)
, AprioriDebugData(..)

, AssocRulesTestData(..)
, AssocRulesTestEntry(..)

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
, tRules         :: [AssocRulesTestEntry set it]
}

data AssocRulesTestEntry set it = AssocRulesTestEntry{
  tRule :: AssocRule set it
, tConf :: Float
, tSup  :: Float
}
