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

, WekaEntryToItemset(..)

, HtmlElem(..)

, AprioriWebAppCache(..)
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
import DataAssociation.Definitions
import DataAssociation.APriori.Public
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

instance (Show WekaDataAttribute, WekaEntryToItemset set it) =>
    ReactiveWebElemSelector WebApp (AprioriWebAppState set it) where

    elemNameParam _ _ = "elem-id"
    reactiveWebElemByName a _ name =
        case name of "raw-data" -> SomeReactiveWebElem . uiRawData $ a

type AprioriWebAppCache set it = AprioriCache set it
type AprioriWebAppState set it = ApplicationState (AprioriWebAppCache set it)
                                                  (MinSupport, MinConfidence)

--uiReactiveWebElems :: WebApp -> [SomeReactiveWebElem (AprioriWebAppState cache)]
--uiReactiveWebElems a = [ SomeReactiveWebElem $ uiRawData a
--                       , SomeReactiveWebElem $ uiConfig a
--                       ]

-----------------------------------------------------------------------------

class HtmlElem e where
    elemHtml :: e -> Html



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

class (Itemset set it, Ord (set it), Ord it) =>
    WekaEntryToItemset set it where
        wekaEntryToItemset :: WekaEntry -> set it

-----------------------------------------------------------------------------

data StatusMsg = StatusMsg{
    msgString     :: String
  , msgType       :: String
  , msgShowMillis :: Maybe Int
  , msgPriority   :: Int
}

data StatusList = StatusList{
    statusShow :: StatusMsg -> IO ()
  , statusHtml :: Html
}

instance HtmlElem StatusList where elemHtml = statusHtml

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


instance HtmlElem            RawDataTextAreaDialog where elemHtml = rawDataHtml
instance ReactiveWebElemConf RawDataTextAreaDialog where reqParam _ = "raw-data"

instance (Show WekaDataAttribute, WekaEntryToItemset set it) =>
    ReactiveWebElem RawDataTextAreaDialog (AprioriWebAppState set it) where

    type ReactiveWebElemArg = [(String, JSValue)]

    reqParse u jobj state = do
        let rawData   = wekaDataFromLines lines
        let sparse   = wekaData2Sparse rawData
        let itemsets = map wekaEntryToItemset sparse

        setRawData state rawData
        setCacheState state $ mkAprioriCache itemsets

        where Just (JSString rawData) = lookup "raw-data" jobj
              lines = map T.unpack . T.splitOn "\n" . T.pack $ fromJSString rawData


-----------------------------------------------------------------------------

newtype AprioriConfigUI = AprioriConfigUI Html

instance HtmlElem            AprioriConfigUI where elemHtml (AprioriConfigUI h) = h
instance ReactiveWebElemConf AprioriConfigUI where reqParam _ = "apriori-params"

--instance ReactiveWebElem AprioriConfigUI (AprioriWebAppState set it) where
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

instance HtmlElem PostProcessFilterBuilderUI where
    elemHtml (PostProcessFilterBuilderUI h) = h

-----------------------------------------------------------------------------

newtype PostProcessSortBuilderUI = PostProcessSortBuilderUI Html

instance HtmlElem PostProcessSortBuilderUI where
    elemHtml (PostProcessSortBuilderUI h) = h

-----------------------------------------------------------------------------

newtype PostProcessGroupBuilderUI = PostProcessGroupBuilderUI Html

instance HtmlElem PostProcessGroupBuilderUI where
    elemHtml (PostProcessGroupBuilderUI h) = h

-----------------------------------------------------------------------------

data ShowProcessedDataUI set it = ShowProcessedDataUI {
    sendDataToUI   :: [[AssocRule set it]] -> IO ()
  , showDataHtml   :: Html
}

instance HtmlElem (ShowProcessedDataUI set it) where elemHtml = showDataHtml

instance ShowUI ShowProcessedDataUI where sendDataToShow = sendDataToUI


