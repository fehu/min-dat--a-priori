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
import DataAssociation.PostProcess.Descriptor

class ExploreProgram cache where

    associationRules :: cache set it
                     -> MinSupport
                     -> MinConfidence
                     -> [PostProcessDescriptor]
                     -> [AssocRule set it]

