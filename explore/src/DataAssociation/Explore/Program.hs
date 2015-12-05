-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.Program
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module DataAssociation.Explore.Program (

) where

import DataAssociation.Definitions
import DataAssociation.PostProcess

class ExploreProgram cache where

    associationRules :: cache set it
                     -> MinSupport
                     -> MinConfidence
                     -> [PostProcessDescriptor set it]
                     -> [AssocRule set it]

