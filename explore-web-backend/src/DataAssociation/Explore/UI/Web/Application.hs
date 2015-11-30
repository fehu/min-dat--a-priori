{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , ExistentialQuantification
           , OverloadedStrings
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

, ReactiveWebElem(..)
, ReactiveWebElemConf(..)
, SomeReactiveWebElem(..)

, AprioriWebAppState(..)

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
import DataAssociation.Explore.UI.State
import WekaData

import Data.IORef
import Data.Text (Text)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Read ( readMaybe )
import Text.Blaze.Html5 (Html)
import qualified Data.Text as T

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


type AprioriWebAppState cache = ApplicationState cache (MinSupport, MinConfidence)

uiReactiveWebElems :: WebApp -> [SomeReactiveWebElem (AprioriWebAppState cache)]
uiReactiveWebElems a = [ SomeReactiveWebElem $ uiRawData a
                       , SomeReactiveWebElem $ uiConfig a
                       ]

-----------------------------------------------------------------------------

type ReqPath = [String]

class ReactiveWebElemConf u where
    reqPath  :: u -> ReqPath
    reqParam :: u -> String

class (ReactiveWebElemConf u) =>
    ReactiveWebElem u state where
        reqParse :: u -> Maybe Text -> state -> state

data SomeReactiveWebElem state = forall u . ReactiveWebElem u state =>
     SomeReactiveWebElem u


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
    rawDataSendDescr :: [(String, String)] -> IO ()
  , rawDataHtml      :: Html
  , rawDataReqPath   :: ReqPath
  , rawDataReqParam  :: String
}

instance RawDataUI RawDataTextAreaDialog where
    sendDataDescription = rawDataSendDescr

instance ReactiveWebElemConf RawDataTextAreaDialog where
    reqPath  = rawDataReqPath
    reqParam = rawDataReqParam

instance ReactiveWebElem RawDataTextAreaDialog (AprioriWebAppState cache) where
    reqParse u mbTxt state = state { rawDataState = wekaDataFromLines lines }
        where lines = maybe [] (map T.unpack . T.splitOn "\n") mbTxt


-----------------------------------------------------------------------------

data AprioriConfigUI = AprioriConfigUI{
    aprioryConfigHtml     :: Html
  , apriofiConfigReqParam :: String
  , apriofiConfigReqPath  :: ReqPath
}

instance ReactiveWebElemConf AprioriConfigUI where
    reqPath = apriofiConfigReqPath
    reqParam = apriofiConfigReqParam

instance ReactiveWebElem AprioriConfigUI (AprioriWebAppState cache) where
    reqParse u mbTxt s = maybe s (\p -> s { programConfigState = p }) mbParams
     where mbParams = do txt <- mbTxt
                         let readMbF x = readMaybe $ T.unpack x :: Maybe Float
                         case T.splitOn "," txt of [s, c] -> do sup  <- readMbF s
                                                                conf <- readMbF c
                                                                return ( MinSupport sup
                                                                       , MinConfidence conf)
                                                   _      -> Nothing


-----------------------------------------------------------------------------

data PostProcessFilterBuilderUI = PostProcessFilterBuilderUI{
--    ppFilterRef     :: IORef (Set RuleFilter)
    ppFilterHtml    :: Html
  , ppFilterReqPath :: ReqPath
}

--instance PostProcessUI PostProcessFilterBuilderUI RuleFilter where
--
--    getPostProcess = fmap Set.toList . readIORef . ppFilterRef
--    setPostProcess u udp = writeIORef (ppFilterRef u) (Set.fromList udp)


-----------------------------------------------------------------------------

data PostProcessSortBuilderUI = PostProcessSortBuilderUI {
--    ppSortRef     :: IORef [RuleOrder]
    ppSortHtml    :: Html
  , ppSortReqPath :: ReqPath
}

--instance PostProcessUI PostProcessSortBuilderUI RuleOrder where
--    getPostProcess = readIORef . ppSortRef
--    setPostProcess = writeIORef . ppSortRef


-----------------------------------------------------------------------------

data PostProcessGroupBuilderUI = PostProcessGroupBuilderUI {
--    ppGroupRef     :: IORef (Maybe RuleGroup)
    ppGroupHtml    :: Html
  , ppGroupReqPath :: ReqPath
}

--instance PostProcessUI PostProcessGroupBuilderUI RuleGroup where
--    getPostProcess = fmap maybeToList . readIORef . ppGroupRef
--    setPostProcess u [upd] = writeIORef (ppGroupRef u) (Just upd)
--    setPostProcess u []    = writeIORef (ppGroupRef u) Nothing

-----------------------------------------------------------------------------

data ShowProcessedDataUI set it = ShowProcessedDataUI {
    sendDataToUI   :: [[AssocRule set it]] -> IO ()
  , showDataHtml   :: Html
}

instance ShowUI ShowProcessedDataUI where
    sendDataToShow = sendDataToUI


