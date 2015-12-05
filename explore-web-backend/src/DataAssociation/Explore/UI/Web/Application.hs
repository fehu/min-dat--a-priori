{-# LANGUAGE TypeFamilies
            , FlexibleInstances
            , ExistentialQuantification
            , OverloadedStrings
            , FlexibleContexts
            , UndecidableInstances
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
import DataAssociation.PostProcess
import DataAssociation.Explore.UI.Application
import DataAssociation.Explore.UI.State
import DataAssociation.Explore.UI.Web.Application.Message
import WekaData

import Data.Maybe
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.UUID    as UUID
import qualified Data.UUID.V4 as UUID

import Control.Applicative ( (<$>) )
import Control.Arrow

import Text.Read ( readMaybe )
import Text.Blaze.Html5 (Html)
import Text.JSON


-----------------------------------------------------------------------------

instance (Itemset Set Item) => ItemByAttribute Set Item where
    containsAnyWithAttr set name = set `containsItem` ItemByAttrExtractor name

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




--instance WithContext RawWekaData where currentContext


instance ReactiveWebElemSelectorParam WebApp where elemNameParam _ = "elem-id"
instance ( Show WekaDataAttribute, Show WekaVal
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
        setRawData state rawData
        msg2UI reporter $ dataUpdateMsg rawData

        let sparse   = wekaData2Sparse rawData
        let itemsets = map wekaEntryToItemset sparse

        putStrLn $ "Read " ++ show (length itemsets) ++ " itemsets."
--        putStrLn $ "itemsets: " ++ show itemsets

        let buildCacheNow = case lookup "build-cache" jobj of Just (JSBool b) -> b
                                                              _               -> False
        if buildCacheNow
            then do
                let cache = mkAprioriCache itemsets

                setCacheState state (cache, itemsets)
                putStrLn $ "setCacheState " ++ show cache
                let AprioriCache c _ = cache
                let cacheMsg = "Created cache with " ++ show (Map.size c) ++ " itemsets."
                putStrLn cacheMsg

                msg2UI reporter $ statusUpdMsg cacheMsg

            else do setCacheState state (emptyAprioriCache, itemsets)
                    msg2UI reporter DoneMsg


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
            case fromJSString msgType of "new"   -> addNewFilter u jobj reporter state
                                         "state" ->
                                            case lookup "state" jobj
                                                of Just (JSBool b) ->
                                                    switchFilter b u jobj reporter state

            where JSString msgType = fromMaybe (error "no message type provided")
                                   $ lookup "post-filter" jobj



addNewFilter u jobj reporter state = do
       let descriptor = read descriptorStr
       filterId <- fmap (("post-filter-" ++) . UUID.toString) UUID.nextRandom
       filters' <- getPostProcess state
       setPostProcess state $ filters' ++ [
            ( ( PostFilterId filterId, PostFilterState True )
            , descriptor :: RuleFilter Item)
         ]
       msg2UI reporter $ NewPostFilter filterId (drop 11 descriptorStr)
    where descriptorStr = fromMaybe (error "failed to read filter " ++ show jobj)
                                    mbDescriptorStr
          mbDescriptorStr = do
              JSString rulePart           <- lookup "rule-side" jobj
              JSArray [JSObject builder]  <- lookup "builder"   jobj
              let itemsetFilter = postProcessFromJObj $ fromJSObject builder
              return $ unwords [ "RuleFilter"
                               , fromJSString rulePart
                               , itemsetFilter]


switchFilter b u jobj reporter state = do
    filters' <- getPostProcess state :: IO [PostFilterEntry]
    let JSString filterId' = fromMaybe (error "No 'filter-id' in message")
                           $ lookup "filter-id" jobj
    let filterId = fromJSString filterId'

    let res = do f <- filters'
                 return $ if postFilterId f == filterId
                             then (postFilterUpdStatus f b, True)
                             else (f, False)

    let (newFilters, updFlags) = unzip res

    case length $ filter id updFlags of 0 -> fail $ "No filter with id '"
                                                  ++ filterId ++ "' found"
                                        1 -> do setPostProcess state newFilters
                                                msg2UI reporter $
                                                       SetPostFilterState filterId b


inParenthesis s  = "(" ++ s ++ ")"
strConstructor = inParenthesis . unwords

postProcessFromJObj [(k, JSString v)] = strConstructor [k, show $ fromJSString v]
postProcessFromJObj [(k, JSArray [JSObject o])] = strConstructor [k, postProcessFromJObj
                                                                     $ fromJSObject o  ]
postProcessFromJObj [(k, JSArray [JSObject l, JSObject r])] =
    strConstructor [k, postProcessFromJObj $ fromJSObject l
                     , postProcessFromJObj $ fromJSObject r ]

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

instance ( AssociationRulesGenerator Set Item, Show WekaVal ) =>
    ReactiveWebElem (ShowProcessedDataUI Set Item) (AprioriWebAppState Set Item) where
        type ReactiveWebElemArg = [(String, JSValue)]
        reqParse u jobj reporter state = do
            (cache, transactions) <- getCacheState state
            (minsup, minconf) <- getProgramConfigState state

            let (newCache, largeSets) = aprioriCached cache transactions minsup
            setCacheState state (newCache, transactions)

            let rules  = generateAssociationRules minconf transactions largeSets

            filtered <- applyFilters state rules -- TODO : more postprocess

            rulesUpdated u reporter state [filtered]



applyFilters state rules = do
    filterDescrs <- getPostProcess state :: IO [PostFilterEntry]
    let filters = map (postProcessFromDescriptor . snd) . filter postFilterEnabled
                $ filterDescrs
    return $ foldr postProcess rules filters

rulesUpdated u reporter state grules = do setCurrentRules state grules
                                          sendDataToShow u reporter grules

-----------------------------------------------------------------------------


