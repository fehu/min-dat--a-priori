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
, ReactiveWebElemSelectorParam(..)

, WekaEntryToItemset(..)

, HtmlElem(..)

, AprioriWebAppCache(..)
, AprioriWebAppState(..)

, WebAppMsg(..)
, Message2UI(..)
, msg2UI

, WebAppStatusMsg(..)
, statusUpdMsg
, statusErrMsg

, StatusList(..)
, RawDataTextAreaDialog(..)
, PostProcessFilterBuilderUI(..)
, PostProcessSortBuilderUI(..)
, PostProcessGroupBuilderUI(..)
, ShowProcessedDataUI(..)

, AprioriConfigUI(..)
, aprioriConfigMSup
, aprioriConfigMConf
, aprioriConfigHtml

, AprioriConfigMSupUI(..)
, AprioriConfigMConfUI(..)

, Item(..)

) where

import DataAssociation
import DataAssociation.Abstract
import DataAssociation.Definitions
import DataAssociation.APriori.Public
import DataAssociation.PostProcess.Descriptor
import DataAssociation.Explore.UI.Application
import DataAssociation.Explore.UI.State
import DataAssociation.Explore.UI.Web.Application.Message
import WekaData

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Applicative ( (<$>) )
import Control.Arrow

import Text.Read ( readMaybe )
import Text.Blaze.Html5 (Html)
import Text.JSON


-----------------------------------------------------------------------------

data WebApp = WebApp RawDataTextAreaDialog
                     AprioriConfigUI
                     PostProcessFilterBuilderUI
                     PostProcessSortBuilderUI
                     PostProcessGroupBuilderUI
                    (ShowProcessedDataUI Set Item)
                     StatusList

instance ApplicationUITypes WebApp where
    type StatusAppUI     = StatusList
    type RawDataAppUI    = RawDataTextAreaDialog
    type ConfigAppUI     = AprioriConfigUI
    type PostFilterAppUI = PostProcessFilterBuilderUI
    type PostSortAppUI   = PostProcessSortBuilderUI
    type PostGroupAppUI  = PostProcessGroupBuilderUI
    type ShowAppUI       = ShowProcessedDataUI Set Item

    type MessagingContext = Message2UI


instance ApplicationUI WebApp where
    uiRawData    (WebApp u _ _ _ _ _ _) = u
    uiConfig     (WebApp _ u _ _ _ _ _) = u
    uiPostFilter (WebApp _ _ u _ _ _ _) = u
    uiPostSort   (WebApp _ _ _ u _ _ _) = u
    uiPostGroup  (WebApp _ _ _ _ u _ _) = u
    uiShow       (WebApp _ _ _ _ _ u _) = u
    uiStatus     (WebApp _ _ _ _ _ _ u) = u

instance ReactiveWebElemSelectorParam WebApp where elemNameParam _ = "elem-id"
instance ( Show WekaDataAttribute
         , WekaEntryToItemset Set Item
         , AssociationRulesGenerator Set Item) =>
    ReactiveWebElemSelector WebApp (AprioriWebAppState Set Item) where
    reactiveWebElemByName a _ name =
        case name of "raw-data"       -> SomeReactiveWebElem $ uiRawData a
                     "min-support"    -> SomeReactiveWebElem . aprioriConfigMSup  . uiConfig $ a
                     "min-confidence" -> SomeReactiveWebElem . aprioriConfigMConf . uiConfig $ a
                     "update-rules"   -> SomeReactiveWebElem $ uiShow a
                     "post-filter"    -> SomeReactiveWebElem $ uiPostFilter a

type AprioriWebAppCache set it = (AprioriCache set it, [set it])
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
        reqParse :: u -> ReactiveWebElemArg -> Message2UI -> state -> IO ()

data SomeReactiveWebElem state = forall u . ReactiveWebElem u state =>
     SomeReactiveWebElem u

class ReactiveWebElemSelectorParam s where elemNameParam :: s -> String
class (ReactiveWebElemSelectorParam s) =>
    ReactiveWebElemSelector s state where
        reactiveWebElemByName :: s -> state -> String -> SomeReactiveWebElem state


-----------------------------------------------------------------------------

class (Itemset set it, Ord (set it), Ord it) =>
    WekaEntryToItemset set it where
        wekaEntryToItemset :: WekaEntry -> set it

-----------------------------------------------------------------------------

data StatusList = StatusList{
    statusShow :: Message2UI -> WebAppStatusMsg -> IO ()
  , statusHtml :: Html
}

instance HtmlElem StatusList where elemHtml = statusHtml

instance StatusUI StatusList where
    type StatusMessage = WebAppStatusMsg

    showStatus = statusShow

-----------------------------------------------------------------------------

data RawDataTextAreaDialog = RawDataTextAreaDialog{
    rawDataSendDescr :: MessagingContext -> RawWekaData -> IO ()
  , rawDataHtml      :: Html
}

instance RawDataUI RawDataTextAreaDialog where
    type RawDataDescription = RawWekaData
    sendDataDescription     = rawDataSendDescr


instance HtmlElem            RawDataTextAreaDialog where elemHtml = rawDataHtml
instance ReactiveWebElemConf RawDataTextAreaDialog where reqParam _ = "raw-data"

instance (Show WekaDataAttribute, WekaEntryToItemset set it) =>
    ReactiveWebElem RawDataTextAreaDialog (AprioriWebAppState set it) where

    type ReactiveWebElemArg = [(String, JSValue)]

    reqParse u jobj reporter state = do
        let rawData = wekaDataFromLines lines
        msg2UI reporter $ dataUpdateMsg rawData

        let sparse   = wekaData2Sparse rawData
        let itemsets = map wekaEntryToItemset sparse

        putStrLn "itemsets"

        let cache = mkAprioriCache itemsets

        setRawData state rawData
        putStrLn $ "Read " ++ show (length itemsets) ++ " itemsets."

        setCacheState state (cache, itemsets)
        putStrLn $ "setCacheState " ++ show cache
        let AprioriCache c = cache
        let cacheMsg = "Created cache with " ++ show (Map.size c) ++ " itemsets."
        putStrLn cacheMsg

        msg2UI reporter $ statusUpdMsg cacheMsg


        where Just (JSString rawData) = lookup "raw-data" jobj
              lines = map T.unpack . T.splitOn "\n" . T.pack $ fromJSString rawData


-----------------------------------------------------------------------------

newtype AprioriConfigUI = AprioriConfigUI (AprioriConfigMSupUI, AprioriConfigMConfUI, Html)

aprioriConfigMSup  (AprioriConfigUI (x,_, _))  = x
aprioriConfigMConf (AprioriConfigUI (_, x, _)) = x
aprioriConfigHtml  (AprioriConfigUI (_, _, x)) = x

instance HtmlElem AprioriConfigUI where elemHtml = aprioriConfigHtml


setAprioriState sel u jobj reporter state = do
    let Ok v' = readJSON . fromJust $ lookup (reqParam u) jobj
    let    v  = read v'
    cState <- getProgramConfigState state
    let newState = sel (const v) cState
    setProgramConfigState state newState
    msg2UI reporter DoneMsg
--    msg2UI reporter . statusUpdMsg $ "Updated apriori parameters: "
--                                   ++ show (fst newState) ++ ", "
--                                   ++ show (snd newState)

-----------------------------------------------------------------------------

newtype AprioriConfigMSupUI = AprioriConfigMSupUI Html

instance HtmlElem            AprioriConfigMSupUI where elemHtml (AprioriConfigMSupUI h) = h
instance ReactiveWebElemConf AprioriConfigMSupUI where reqParam _ = "apriori-param"

instance (Show WekaDataAttribute, WekaEntryToItemset set it) =>
    ReactiveWebElem AprioriConfigMSupUI (AprioriWebAppState set it) where
        type ReactiveWebElemArg = [(String, JSValue)]
        reqParse = setAprioriState (first . (.) MinSupport)

-----------------------------------------------------------------------------

newtype AprioriConfigMConfUI = AprioriConfigMConfUI Html

instance HtmlElem            AprioriConfigMConfUI where elemHtml (AprioriConfigMConfUI h) = h
instance ReactiveWebElemConf AprioriConfigMConfUI where reqParam _ = "apriori-param"

instance (Show WekaDataAttribute, WekaEntryToItemset set it) =>
    ReactiveWebElem AprioriConfigMConfUI (AprioriWebAppState set it) where
        type ReactiveWebElemArg = [(String, JSValue)]
        reqParse = setAprioriState (second . (.) MinConfidence)

-----------------------------------------------------------------------------

newtype PostProcessFilterBuilderUI = PostProcessFilterBuilderUI Html

instance HtmlElem PostProcessFilterBuilderUI where
    elemHtml (PostProcessFilterBuilderUI h) = h

instance ReactiveWebElemConf PostProcessFilterBuilderUI where reqParam _ = undefined -- "builder"

instance (Show WekaDataAttribute, WekaEntryToItemset set it) =>
    ReactiveWebElem PostProcessFilterBuilderUI (AprioriWebAppState set it) where
        type ReactiveWebElemArg = [(String, JSValue)]
        reqParse u jobj reporter state =
            do putStrLn $ "jobj = " ++ show jobj
               putStrLn $ "descriptorStr = " ++ show descriptorStr
               let descriptor = read descriptorStr
               putStrLn $ "descriptor = " ++ show descriptor
               setPostProcess state [descriptor :: RuleFilter Item]
               msg2UI reporter DoneMsg
            where descriptorStr = fromMaybe (error "failed to read filter " ++ show jobj)
                                            mbDescriptorStr
                  mbDescriptorStr = do
                      JSString rulePart           <- lookup "rule-side" jobj
                      JSArray [JSObject builder]  <- lookup "builder"   jobj
                      let itemsetFilter = postProcessFromJObj $ fromJSObject builder
                      return $ strConstructor [ "RuleFilter"
                                              , fromJSString rulePart
                                              , itemsetFilter]


inParenthesis s  = "(" ++ s ++ ")"
strConstructor = inParenthesis . unwords

postProcessFromJObj [(k, JSString v)] = strConstructor [k, show $ fromJSString v]
postProcessFromJObj [(k, JSArray [JSObject o])] = strConstructor [k, postProcessFromJObj
                                                                     $ fromJSObject o  ]
postProcessFromJObj [(k, JSArray [JSObject l, JSObject r])] =
    strConstructor[k, postProcessFromJObj $ fromJSObject l
                    , postProcessFromJObj $ fromJSObject r]

postProcessFromJObj [] = ""

postProcessFromJObj x = error $ "postProcessFromJObj: not matched " ++ show x

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
    sendDataToUI   :: Message2UI -> [[AssocRule set it]] -> IO ()
  , showDataHtml   :: Html
}

instance HtmlElem (ShowProcessedDataUI set it) where elemHtml = showDataHtml

instance ShowUI ShowProcessedDataUI where sendDataToShow = sendDataToUI

instance ReactiveWebElemConf (ShowProcessedDataUI set it) where reqParam = const ""

instance (AssociationRulesGenerator Set Item) =>
    ReactiveWebElem (ShowProcessedDataUI Set Item) (AprioriWebAppState Set Item) where
        type ReactiveWebElemArg = [(String, JSValue)]
        reqParse u jobj reporter state = do
            (cache, transactions) <- getCacheState state
            (minsup, minconf) <- getProgramConfigState state
            let largeSets = aprioriCached cache minsup
            let rules  = generateAssociationRules minconf transactions largeSets
            setCurrentRules state [rules]   -- TODO : Postprocess
            sendDataToShow u reporter [rules]

-----------------------------------------------------------------------------


