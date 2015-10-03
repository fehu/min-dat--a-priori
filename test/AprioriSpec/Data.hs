-----------------------------------------------------------------------------
-- Module      :  AprioriSpec.Data
-- License     :  MIT
-----------------------------------------------------------------------------

module AprioriSpec.Data (

  AprioriTestData(..)
, AprioriTestRun(..)

) where


import DataAssociation.Definitions


data AprioriTestData = AprioriTestData{
  tTransactions :: [[String]]
, tMinsup  :: MinSupport
, tMinconf :: MinConfidence
, tRuns :: [AprioriTestRun]
}

data AprioriTestRun = AprioriTestRun{
  tSeeds :: [([String], Int)]
, tJoin  :: [[String]]
, tPrune :: [[String]]
}



