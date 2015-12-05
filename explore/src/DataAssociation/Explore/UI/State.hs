{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , DeriveDataTypeable
           , FlexibleContexts
           , UndecidableInstances
       #-}

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

, PostFilterId(..)
, PostFilterState(..)
, PostFilterEntry
, postFilterId
, postFilterEnabled
, postFilterUpdStatus

, getProgramConfigState
, setProgramConfigState

, getCacheState
, setCacheState

, getCurrentRules
, setCurrentRules

, Item(..)

) where

import DataAssociation.Definitions
import DataAssociation.PostProcess
import DataAssociation.Explore.UI.Application
import WekaData

import Data.IORef
import Data.Typeable
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

data Item = ReadItem String
          | Item String WekaDataAttribute
          | SingletonDomainItem String
          | ItemByAttrExtractor String
          deriving Typeable

instance (Show WekaVal) => Show Item where show (ReadItem s) = s
                                           show (Item v _)   = v
                                           show (SingletonDomainItem s) = s
instance Read Item where readsPrec d r = [(ReadItem x, y)]
                                   where [(x, y)] = readsPrec d r

instance Eq Item where

    (ReadItem r) == (Item v _)              = r == v
    (ReadItem r) == (SingletonDomainItem v) = r == v

    t@(Item _ _)              == r@(ReadItem _) = r == t
    t@(SingletonDomainItem _) == r@(ReadItem _) = r == t

    (ItemByAttrExtractor e) == (Item _ a)              = e == wekaAttributeName a
    (ItemByAttrExtractor e) == (SingletonDomainItem v) = e == v

    t@(Item _ _)              == e@(ItemByAttrExtractor _) = e == t
    t@(SingletonDomainItem _) == e@(ItemByAttrExtractor _) = e == t

    (Item x _)              == (Item y _)              = x == y
    (SingletonDomainItem x) == (SingletonDomainItem y) = x == y

    (Item x _)              == (SingletonDomainItem y) = x == y
    (SingletonDomainItem x) == (Item y _)              = x == y

    _ == _ = False

instance Ord Item where

    (ReadItem r) `compare` (Item v _)              = r `compare` v
    (ReadItem r) `compare` (SingletonDomainItem v) = r `compare` v

    t@(Item _ _)              `compare` r@(ReadItem _) = r `compare` t
    t@(SingletonDomainItem _) `compare` r@(ReadItem _) = r `compare` t

    (ItemByAttrExtractor e) `compare` (Item _ a)              = e `compare` wekaAttributeName a
    (ItemByAttrExtractor e) `compare` (SingletonDomainItem v) = e `compare` v

    t@(Item _ _)              `compare` e@(ItemByAttrExtractor _) = e `compare` t
    t@(SingletonDomainItem _) `compare` e@(ItemByAttrExtractor _) = e `compare` t

    (Item x _)              `compare` (Item y _)              = x `compare` y
    (SingletonDomainItem x) `compare` (SingletonDomainItem y) = x `compare` y

    (Item x _)              `compare` (SingletonDomainItem y) = x `compare` y
    (SingletonDomainItem x) `compare` (Item y _)              = x `compare` y


-----------------------------------------------------------------------------

newtype PostFilterId    = PostFilterId String  deriving (Show, Eq, Ord)
newtype PostFilterState = PostFilterState Bool deriving (Show, Eq, Ord)

postFilterId ((PostFilterId id, _), _) = id
postFilterEnabled ((_, PostFilterState b), _) = b
postFilterUpdStatus ((id, _), f) b = ((id, PostFilterState b), f)

type PostFilterEntry = ((PostFilterId, PostFilterState), RuleFilter Item)

-----------------------------------------------------------------------------

data ApplicationState cache conf = ApplicationState{
    cacheState                  :: IORef cache
  , rawDataState                :: IORef RawWekaData
  , programConfigState          :: IORef conf
  , postProcessFilterState      :: IORef (Set PostFilterEntry)
  , postProcessSortState        :: IORef [RuleOrder]
  , postProcessGroupState       :: IORef (Maybe RuleGroup)
  , currentRules                :: IORef [[AssocRule Set Item]]
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
    sRules <- newIORef []

    return $ ApplicationState sCache sData sConf sPPF sPPS sPPG sRules

instance RawDataInnerRepr (ApplicationState cache conf) where
    type RawData = RawWekaData
    getRawData = readIORef . rawDataState
    setRawData = writeIORef . rawDataState

instance PostProcessInnerRepr (ApplicationState cache conf) PostFilterEntry where
    getPostProcess = fmap Set.toList . readIORef . postProcessFilterState
    setPostProcess s = writeIORef (postProcessFilterState s) . Set.fromList

instance PostProcessInnerRepr (ApplicationState cache conf) (RuleOrder) where
    getPostProcess = readIORef . postProcessSortState
    setPostProcess = writeIORef . postProcessSortState

instance PostProcessInnerRepr (ApplicationState cache conf) RuleGroup where
    getPostProcess = fmap maybeToList . readIORef . postProcessGroupState
    setPostProcess s = writeIORef (postProcessGroupState s) . listToMaybe

getProgramConfigState = readIORef . programConfigState
setProgramConfigState = writeIORef . programConfigState

getCacheState = readIORef . cacheState
setCacheState = writeIORef . cacheState

getCurrentRules = readIORef . currentRules
setCurrentRules = writeIORef . currentRules

