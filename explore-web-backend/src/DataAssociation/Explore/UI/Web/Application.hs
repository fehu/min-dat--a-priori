{-# LANGUAGE TypeFamilies
           , FlexibleInstances
--           , ExistentialQuantification
         #-}

-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.UI.Web.Application
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--

module DataAssociation.Explore.UI.Web.Application (

  WebApp(..)

, StatusMsg(..)
, StatusList(..)
, RawDataTextAreaDialog(..)
, PostProcessFilterBuilderUI(..)
, PostProcessSortBuilderUI(..)
, PostProcessGroupBuilderUI(..)
, ShowProcessedDataUI(..)

, AprioriConfigUI(..)

, ReqPath



) where

import DataAssociation
import DataAssociation.PostProcess.Descriptor
import DataAssociation.Explore.UI.Application

import Data.IORef
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Blaze.Html5 (Html)

--import Text.Blaze.Html5 hiding (map, head)
--import qualified Text.Blaze.Html5 as H
--import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
--import qualified Text.Blaze.Html5.Attributes as A

-----------------------------------------------------------------------------

data WebApp = WebApp RawDataTextAreaDialog
                     AprioriConfigUI
                     PostProcessFilterBuilderUI
                     PostProcessSortBuilderUI
                     PostProcessGroupBuilderUI
                    (ShowProcessedDataUI Set String)
                     StatusList

instance ApplicationUITypes WebApp where
    type StatusAppUI     = StatusList
    type RawDataAppUI    = RawDataTextAreaDialog
    type ConfigAppUI     = AprioriConfigUI
    type PostFilterAppUI = PostProcessFilterBuilderUI
    type PostSortAppUI   = PostProcessSortBuilderUI
    type PostGroupAppUI  = PostProcessGroupBuilderUI
    type ShowAppUI       = ShowProcessedDataUI Set String


instance ApplicationUI WebApp where
    uiRawData    (WebApp u _ _ _ _ _ _) = u
    uiConfig     (WebApp _ u _ _ _ _ _) = u
    uiPostFilter (WebApp _ _ u _ _ _ _) = u
    uiPostSort   (WebApp _ _ _ u _ _ _) = u
    uiPostGroup  (WebApp _ _ _ _ u _ _) = u
    uiShow       (WebApp _ _ _ _ _ u _) = u
    uiStatus     (WebApp _ _ _ _ _ _ u) = u


-----------------------------------------------------------------------------

type ReqPath = [String]

-----------------------------------------------------------------------------

data StatusMsg = StatusMsg{
    msgString     :: String
  , msgShowMillis :: Maybe Int
  , msgPriority   :: Int
}

data StatusList = StatusList{
    statusShow :: StatusMsg -> IO ()
  , statusHtml :: Html
}

instance StatusUI StatusList where
    type StatusMessage = StatusMsg
    showStatus = statusShow

-----------------------------------------------------------------------------

data RawDataTextAreaDialog = RawDataTextAreaDialog{
    rawDataRef      :: IORef [String]
  , rawDataHtml     :: Html
  , rawDataReqPath  :: ReqPath
}

instance RawDataUI RawDataTextAreaDialog where
    type RawData = [String]

    getRawData = readIORef . rawDataRef
    setRawData = writeIORef . rawDataRef

-----------------------------------------------------------------------------

data AprioriConfigUI = AprioriConfigUI{
    aprioriMinSupRef     :: IORef MinSupport
  , aprioriMinConfRef    :: IORef MinConfidence
  , aprioryConfigHtml    :: Html
  , apriofiConfigReqPath :: ReqPath
}

-----------------------------------------------------------------------------

data PostProcessFilterBuilderUI = PostProcessFilterBuilderUI{
    ppFilterRef     :: IORef (Set RuleFilter)
  , ppFilterHtml    :: Html
  , ppFilterReqPath :: ReqPath
}

instance PostProcessUI PostProcessFilterBuilderUI RuleFilter where

    getPostProcess = fmap Set.toList . readIORef . ppFilterRef
    setPostProcess u udp = writeIORef (ppFilterRef u) (Set.fromList udp)


-----------------------------------------------------------------------------

data PostProcessSortBuilderUI = PostProcessSortBuilderUI {
    ppSortRef     :: IORef [RuleOrder]
  , ppSortHtml    :: Html
  , ppSortReqPath :: ReqPath
}

instance PostProcessUI PostProcessSortBuilderUI RuleOrder where
    getPostProcess = readIORef . ppSortRef
    setPostProcess = writeIORef . ppSortRef


-----------------------------------------------------------------------------

data PostProcessGroupBuilderUI = PostProcessGroupBuilderUI {
    ppGroupRef     :: IORef (Maybe RuleGroup)
  , ppGroupHtml    :: Html
  , ppGroupReqPath :: ReqPath
}

instance PostProcessUI PostProcessGroupBuilderUI RuleGroup where
    getPostProcess = fmap maybeToList . readIORef . ppGroupRef
    setPostProcess u [upd] = writeIORef (ppGroupRef u) (Just upd)
    setPostProcess u []    = writeIORef (ppGroupRef u) Nothing

-----------------------------------------------------------------------------

data ShowProcessedDataUI set it = ShowProcessedDataUI {
    sendDataToUI   :: [[AssocRule set it]] -> IO ()
  , showDataHtml   :: Html
}

instance ShowUI ShowProcessedDataUI where
    sendDataToShow = sendDataToUI


