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
, newState
, InitialState(..)
, newStateWithInitial

, getProgramConfigState
, setProgramConfigState

, getCacheState
, setCacheState

) where

--import Control.Concurrent.MVar

import DataAssociation
import DataAssociation.PostProcess.Descriptor
import DataAssociation.Explore.UI.Application
import WekaData

import Data.IORef
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

data ApplicationState cache conf = ApplicationState{
    cacheState                  :: IORef cache
  , rawDataState                :: IORef RawWekaData
  , programConfigState          :: IORef conf
  , postProcessFilterState      :: IORef (Set RuleFilter)
  , postProcessSortState        :: IORef [RuleOrder]
  , postProcessGroupState       :: IORef (Maybe RuleGroup)
}

data InitialState cache wData conf = InitialState cache wData conf

newStateWithInitial (InitialState cache wData conf) = newState cache wData conf

newState cache wData conf = do
    sCache <- newIORef cache
    sData  <- newIORef wData
    sConf  <- newIORef conf
    sPPF   <- newIORef Set.empty
    sPPS   <- newIORef []
    sPPG   <- newIORef Nothing

    return $ ApplicationState sCache sData sConf sPPF sPPS sPPG

instance RawDataInnerRepr (ApplicationState cache conf) where
    type RawData = RawWekaData
    getRawData = readIORef . rawDataState
    setRawData = writeIORef . rawDataState

instance PostProcessInnerRepr (ApplicationState cache conf) RuleFilter where
    getPostProcess = fmap Set.toList . readIORef . postProcessFilterState
    setPostProcess s = writeIORef (postProcessFilterState s) . Set.fromList

instance PostProcessInnerRepr (ApplicationState cache conf) RuleOrder where
    getPostProcess = readIORef . postProcessSortState
    setPostProcess = writeIORef . postProcessSortState

instance PostProcessInnerRepr (ApplicationState cache conf) RuleGroup where
    getPostProcess = fmap maybeToList . readIORef . postProcessGroupState
    setPostProcess s = writeIORef (postProcessGroupState s) . listToMaybe

getProgramConfigState = readIORef . programConfigState
setProgramConfigState = writeIORef . programConfigState

getCacheState = readIORef . cacheState
setCacheState = writeIORef . cacheState

