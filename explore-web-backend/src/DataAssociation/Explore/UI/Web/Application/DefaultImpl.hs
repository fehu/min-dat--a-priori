{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.UI.Web.Application.DefaultImpl
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module DataAssociation.Explore.UI.Web.Application.DefaultImpl (

  webApp

, statusList
, rawDataTextAreaDialog
, aprioriConfigUI
, postProcessFilterBuilderUI
, postProcessSortBuilderUI
, postProcessGroupBuilderUI
, showProcessedDataUI


) where

import Prelude hiding (head, div, span)
import DataAssociation
import DataAssociation.Explore.UI.Web.Application
import DataAssociation.Explore.UI.Web.Render

import Data.IORef
import qualified Data.Set as Set

import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A



webApp = WebApp rawDataTextAreaDialog
                aprioriConfigUI
                postProcessFilterBuilderUI
                postProcessSortBuilderUI
                postProcessGroupBuilderUI
                showProcessedDataUI
                statusList

instance RenderableWebPage WebApp where
    reqPath _ = ["explore", "apriori"]

    renderWebPage app = html $ do
        pageHead
        body $ do
            header $ h1 "Explore: Apriori"
            section ! A.id "config" $ do
                dataConfigHtml
                filterConfigHtml
                sortAndGroupConfigHtml
            section ! A.id "apply" $
                button "Apply" ! A.onclick loadButtonClicked
            section ! A.id "rules" $ do
                h2 "Rules"
                span "TODO" ! A.class_ "todo"


pageHead = head $ do
    link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/apriori.css"
    script "" ! A.src "//code.jquery.com/jquery-2.1.4.min.js"
    script "var wSocket = new WebSocket('ws://localhost:9160/')"



dataConfigHtml = div ! A.class_ "data" $ do
    h3 "Data"
    button "Load Data" ! A.onclick loadDataClicked
    hr
    span "" ! A.class_ "info"

filterConfigHtml = div ! A.class_ "filter" $ do
    h3 "Filters"
    span "TODO" ! A.class_ "todo"

sortAndGroupConfigHtml = div ! A.class_ "sort-group" $ do
    h3 "Sort"
    span "TODO" ! A.class_ "todo"
    hr
    h3 "Group"
    span "TODO" ! A.class_ "todo"


loadDataClicked   = "TODO: upload data"
loadButtonClicked = "TODO: apply changes"



statusList = StatusList{
    statusShow = undefined
  , statusHtml = undefined
}

rawDataTextAreaDialog = RawDataTextAreaDialog{
    rawDataSendDescr = undefined
  , rawDataHtml      = undefined
}


aprioriConfigUI = AprioriConfigUI undefined

postProcessFilterBuilderUI = PostProcessFilterBuilderUI undefined

postProcessSortBuilderUI = PostProcessSortBuilderUI undefined

postProcessGroupBuilderUI = PostProcessGroupBuilderUI undefined

showProcessedDataUI = ShowProcessedDataUI{
    sendDataToUI = undefined,
    showDataHtml = undefined
}







