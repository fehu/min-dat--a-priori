-- |
--
-- Module      :  AprioriSpec.Data
-- License     :  MIT


module AprioriSpec.Data (

  AprioriTestData(..)
, AprioriDebugData(..)

) where


import DataAssociation.Definitions
import DataAssociation.APriori.Debug

data AprioriTestData set it = AprioriTestData{
  tTransactions :: [set it]
, tMinsup  :: MinSupport
, tMinconf :: MinConfidence
, tRuns    :: [AprioriDebugData set it]
}
