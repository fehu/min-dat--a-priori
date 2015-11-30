{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.UI.State
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

module DataAssociation.Explore.UI.State (

  ApplicationState(..)

--, getProgramConfigState
--, setProgramConfigState

) where

--import GHC.Conc
--import Control.Monad

--import DataAssociation
import DataAssociation.PostProcess.Descriptor
--import DataAssociation.Explore.UI.Application
import WekaData

--import Data.Maybe
import Data.Set (Set)
--import qualified Data.Set as Set

-----------------------------------------------------------------------------

data ApplicationState cache conf = ApplicationState{
    cacheState                  :: cache
  , rawDataState                :: RawWekaData
  , programConfigState          :: conf
--  , aprioriMinSupportState      :: MinSupport
--  , aprioriMinConfidenceState   :: MinConfidence
  , postProcessFilterState      :: Set RuleFilter
  , postProcessSortState        :: [RuleOrder]
  , postProcessGroupState       :: Maybe RuleGroup
}


--writeState sel s = void . atomically . writeTVar (sel s)

--instance RawDataInnerRepr (ApplicationState cache conf) where
--    type RawData = [String]
--    getRawData = readTVarIO . rawDataState
--    setRawData = writeState rawDataState
--
--instance PostProcessInnerRepr (ApplicationState cache conf) RuleFilter where
--    getPostProcess = fmap Set.toList . readTVarIO . postProcessFilterState
--    setPostProcess s = writeState postProcessFilterState s . Set.fromList
--
--instance PostProcessInnerRepr (ApplicationState cache conf) RuleOrder where
--    getPostProcess = readTVarIO . postProcessSortState
--    setPostProcess = writeState postProcessSortState
--
--instance PostProcessInnerRepr (ApplicationState cache conf) RuleGroup where
--    getPostProcess = fmap maybeToList . readTVarIO . postProcessGroupState
--    setPostProcess s = writeState postProcessGroupState s . listToMaybe

--getProgramConfigState = readTVarIO . programConfigState
--setProgramConfigState = writeState programConfigState


