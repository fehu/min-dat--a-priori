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

) where

import DataAssociation
import DataAssociation.PostProcess.Descriptor
import DataAssociation.Explore.UI.Application

import Data.IORef
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

instance UIApplicationTypes WebApp where
    type RawDataAppUI    = RawDataTextAreaDialog
    type ConfigAppUI     = AprioriConfigUI
    type PostFilterAppUI = PostProcessFilterBuilderUI
    type PostSortAppUI   = PostProcessSortBuilderUI
    type PostGroupAppUI  = PostProcessGroupBuilderUI
    type ShowAppUI       = ShowProcessedDataUI Set String


instance UIApplication WebApp where
    uiRawData    (WebApp u _ _ _ _ _) = u
    uiConfig     (WebApp _ u _ _ _ _) = u
    uiPostFilter (WebApp _ _ u _ _ _) = u
    uiPostSort   (WebApp _ _ _ u _ _) = u
    uiPostGroup  (WebApp _ _ _ _ u _) = u
    uiShow       (WebApp _ _ _ _ _ u) = u


-----------------------------------------------------------------------------

type ReqPath = [String]

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
    ppGroupRef     :: IORef RuleGroup
  , ppGroupHtml    :: Html
  , ppGroupReqPath :: ReqPath
}

instance PostProcessUI PostProcessGroupBuilderUI RuleGroup where
    getPostProcess = fmap (:[]) . readIORef . ppGroupRef
    setPostProcess u [upd] = writeIORef (ppGroupRef u) upd

-----------------------------------------------------------------------------

data ShowProcessedDataUI set it = ShowProcessedDataUI {
    sendDataToUI   :: [[AssocRule set it]] -> IO ()
  , showDataHtml   :: Html
}

instance ShowUI ShowProcessedDataUI where
    sendDataToShow = sendDataToUI


