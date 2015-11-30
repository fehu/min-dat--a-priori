{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , ExistentialQuantification
           , OverloadedStrings
           , FlexibleContexts
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
, ReactiveWebElemSelector(..)

, AprioriWebAppState(..)

, StatusMsg(..)
, StatusList(..)
, RawDataTextAreaDialog(..)
, PostProcessFilterBuilderUI(..)
, PostProcessSortBuilderUI(..)
, PostProcessGroupBuilderUI(..)
, ShowProcessedDataUI(..)

, AprioriConfigUI(..)

) where

import DataAssociation
import DataAssociation.PostProcess.Descriptor
import DataAssociation.Explore.UI.Application
import DataAssociation.Explore.UI.State
import WekaData

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Text.Read ( readMaybe )
import Text.Blaze.Html5 (Html)

import Text.JSON

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

instance (Show WekaDataAttribute) =>
    ReactiveWebElemSelector WebApp (AprioriWebAppState cache) where

    elemNameParam _ _ = "elem-id"
    reactiveWebElemByName a _ name =
        case name of "raw-data" -> SomeReactiveWebElem . uiRawData $ a

type AprioriWebAppState cache = ApplicationState cache (MinSupport, MinConfidence)

--uiReactiveWebElems :: WebApp -> [SomeReactiveWebElem (AprioriWebAppState cache)]
--uiReactiveWebElems a = [ SomeReactiveWebElem $ uiRawData a
--                       , SomeReactiveWebElem $ uiConfig a
--                       ]

-----------------------------------------------------------------------------


class ReactiveWebElemConf u where
    reqParam :: u -> String

class (ReactiveWebElemConf u) =>
    ReactiveWebElem u state where
        type ReactiveWebElemArg
        reqParse :: u -> ReactiveWebElemArg -> state -> IO ()

data SomeReactiveWebElem state = forall u . ReactiveWebElem u state =>
     SomeReactiveWebElem u

class ReactiveWebElemSelector s state where
    elemNameParam         :: s -> state -> String
    reactiveWebElemByName :: s -> state -> String -> SomeReactiveWebElem state

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
}

instance RawDataUI RawDataTextAreaDialog where
    sendDataDescription = rawDataSendDescr




instance ReactiveWebElemConf RawDataTextAreaDialog where reqParam _ = "raw-data"

instance (Show WekaDataAttribute) =>
    ReactiveWebElem RawDataTextAreaDialog (AprioriWebAppState cache) where

    type ReactiveWebElemArg = [(String, JSValue)]

    reqParse u jobj state = setRawData state $ wekaDataFromLines lines
        where Just (JSString rawData) = lookup "raw-data" jobj
              lines = map T.unpack . T.splitOn "\n" . T.pack $ fromJSString rawData


-----------------------------------------------------------------------------

newtype AprioriConfigUI = AprioriConfigUI Html

instance ReactiveWebElemConf AprioriConfigUI where reqParam _ = "apriori-params"

--instance ReactiveWebElem AprioriConfigUI (AprioriWebAppState cache) where
--    reqParse u mbTxt s = maybe s (\p -> s { programConfigState = p }) mbParams
--     where mbParams = do txt <- mbTxt
--                         let readMbF x = readMaybe $ T.unpack x :: Maybe Float
--                         case T.splitOn "," txt of [s, c] -> do sup  <- readMbF s
--                                                                conf <- readMbF c
--                                                                return ( MinSupport sup
--                                                                       , MinConfidence conf)
--                                                   _      -> Nothing
--

-----------------------------------------------------------------------------

newtype PostProcessFilterBuilderUI = PostProcessFilterBuilderUI Html

-----------------------------------------------------------------------------

newtype PostProcessSortBuilderUI = PostProcessSortBuilderUI Html

-----------------------------------------------------------------------------

newtype PostProcessGroupBuilderUI = PostProcessGroupBuilderUI Html

-----------------------------------------------------------------------------

data ShowProcessedDataUI set it = ShowProcessedDataUI {
    sendDataToUI   :: [[AssocRule set it]] -> IO ()
  , showDataHtml   :: Html
}

instance ShowUI ShowProcessedDataUI where sendDataToShow = sendDataToUI


