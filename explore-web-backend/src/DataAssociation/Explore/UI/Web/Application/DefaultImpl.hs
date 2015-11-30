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
import DataAssociation.Explore.UI.Application
import DataAssociation.Explore.UI.Web.Application
import DataAssociation.Explore.UI.Web.Render

import Data.IORef
import qualified Data.Set as Set
import Data.List (intercalate)

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

    renderWebPage app = -- do
--        docType
        html $ do
            pageHead
            body $ do
                pageScripts
                header $ h1 "Explore: Apriori"
                elemHtml $ uiStatus app
                div ! A.class_ "container" $ do
                    section ! A.id "config"
                            ! A.class_ "span9" $
                        div ! A.class_ "row" $ do
--                            elemHtml $ uiRawData app
                            divFor2 "raw-data-and-conf"
                                    "Data"    (uiRawData app)
                                    "Apriori" (uiConfig app)
                            elemHtml $ uiPostFilter app
                            divFor2 "sort-group"
                                    "Sort"  (uiPostSort app)
                                    "Group" (uiPostGroup app)
                    hr
                    section ! A.id "apply"
                            ! A.class_ "span9" $
                        button "Apply" ! A.onclick loadButtonClicked
                    section ! A.id "rules"
                            ! A.class_ "span9" $ do
                        h2 "Rules"
                        elemHtml $ uiShow app
            loadDataDialog


pageHead = head $ do
--    link ! A.rel "stylesheet"
--         ! A.href "https://necolas.github.io/normalize.css/3.0.2/normalize.css"
--
    link ! A.rel "stylesheet"
         ! A.type_ "text/css"
         ! A.href "/static/apriori.css"

    link ! A.rel "stylesheet"
         ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css"
    link ! A.rel "stylesheet"
         ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"


pageScripts = do
    script "" ! A.src "//code.jquery.com/jquery-2.1.4.min.js"
    script "" ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"

    script $ toHtml scriptsJS

scriptsJS :: String
scriptsJS = "\nvar wSocket = new WebSocket('ws://localhost:9160/'); \n\
            \wSocket.onclose = function(e) { alert('Connction to server closed!') };\n" ++
            statusScriptsJS ++
            "wSocket.onmessage = " ++ handleWsMessageJS ++ ";\n"


statusScriptsJS = " closeBtn = '&lt;a class=\"close\" data-dismiss=\"alert\" \
                  \                   href=\"#\"&gt;&times;&lt;/a&gt;'; \n\
                  \ var showStatus = function(type, msg){     \n\
                  \     var clazz = '';                       \n\
                  \     var extra = '';                       \n\
                  \     switch (type) {                       \n\
                  \         case 'error':                     \n\
                  \             clazz = 'alert-error';        \n\
                  \             extra = '&lt;h4 class=\"alert-heading\"&gt;Error!&lt;/h4&gt;'; \n\
                  \             break;                        \n\
                  \         case 'ok': break;                 \n\
                  \       };                                  \n\
                  \     str = '&lt;div class=\"alert fade in '.concat(clazz).concat('\"&gt;');\n\
                  \     str = str.concat(closeBtn).concat(msg).concat('&lt;/div&gt;');        \n\
                  \     $(str).appendTo('#statuses');         \n\
                  \  }                                        \n\
                  \"

handleWsMessageJS = "function(msg) {\n\
    \ obj = JSON.parse(msg);                    \n\
    \ switch(obj['type']) {                     \n\
    \   case 'error':                           \n\
    \        showStatus('error', obj['error']); \n\
    \        break;                             \n\
    \   case 'status':                          \n\
    \       showStatus('ok', obj['status']);    \n\
    \       break;                              \n\
    \   case 'data-info':                       \n\
    \       setDataInfo(obj['data-info']);      \n\
    \       break;                              \n\
    \   case 'rules':                           \n\
    \       rulesUpdate(obj['rules']);          \n\
    \       break;                              \n\
    \  }                                        \n\
    \ }"


divFor2 id n1 e1 n2 e2 =
    div ! A.class_ (stringValue $ id ++ " col-md-4") $ do
        h3 n1
        elemHtml e1
        h3 n2
        elemHtml e2

--sortAndGroupConfigHtml = div ! A.class_ "sort-group col-md-4" $ do
--    h3 "Sort"
--
--    hr
--    h3 "Group"
--    span "TODO" ! A.class_ "todo"

sendMessageObjJS :: [(String, String)] -> String
sendMessageObjJS entries = "wSocket.send(JSON.stringify({" ++ obj ++ "}))"
    where obj = intercalate ","
              $ Prelude.map (\(k,v) -> show k ++ ":" ++ v) entries

loadDataDialog = div     ! A.id "upload-data-dialog"
                         ! A.class_ "modal fade"
                         ! A.tabindex "-1"
                         ! customAttribute "role" "dialog"
                         ! customAttribute "aria-labelledby" "upload-data-dialog-label"
                         ! customAttribute "aria-hidden" "true"
                   $ div ! A.class_ "modal-dialog"
                   $ div ! A.class_ "modal-content"
                   $ do div ! A.class_ "modal-header"
                            $ do mkBootstrapCloseModalButton "×" "close"
                                 h3 "Upload Data"
                                    ! A.id "upload-data-dialog-label"
                        div ! A.class_ "modal-body" $ do
                            p "Enter the data in ARFF format:"
                            textarea "" ! A.id "upload-data-dialog-text"
                        div ! A.class_ "modal-footer" $ do
                            mkBootstrapCloseModalButton "Upload" "btn btn-primary"
                                ! A.onclick (stringValue $ sendMessageObjJS
                                             [ ("elem-id", "\"raw-data\"")
                                             , ("raw-data", "$('#upload-data-dialog-text').val()")
                                             ]
                                            )
                            mkBootstrapCloseModalButton "Cancel" "btn btn-inverse"


mkBootstrapButton txt clazz = button txt ! A.type_ "button"
                                         ! A.class_ clazz

mkBootstrapCloseModalButton txt clazz =
    mkBootstrapButton txt clazz
        ! customAttribute "data-dismiss" "modal"
        ! customAttribute "aria-hidden" "true"

-- <h3 id="myModalLabel">Modal header</h3>

--loadDataClicked   = "TODO: upload data"
loadButtonClicked = "TODO: apply changes"



statusList = StatusList{
    statusShow = undefined
  , statusHtml = div "" ! A.id "statuses"  -- span "TODO: Status" ! A.class_ "todo"
}

rawDataTextAreaDialog = RawDataTextAreaDialog{
    rawDataSendDescr = undefined

  , rawDataHtml = do -- div ! A.class_ "data col-md-4" $ do
        mkBootstrapButton "Load Data" "btn"
            ! customAttribute "data-toggle" "modal"
            ! customAttribute "data-target" "#upload-data-dialog"
        hr
        span "" ! A.class_ "info"
}


aprioriConfigUI = AprioriConfigUI $
    span "TODO" ! A.class_ "todo"

postProcessFilterBuilderUI = PostProcessFilterBuilderUI $
    div ! A.class_ "filter col-md-4" $
        do h3 "Filters"
           span "TODO" ! A.class_ "todo"

postProcessSortBuilderUI = PostProcessSortBuilderUI $
    span "TODO" ! A.class_ "todo"

postProcessGroupBuilderUI = PostProcessGroupBuilderUI $
    span "TODO" ! A.class_ "todo"

showProcessedDataUI = ShowProcessedDataUI{
    sendDataToUI = undefined,
    showDataHtml = span "TODO" ! A.class_ "todo"
}







