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

    renderWebPage app =
        html $ do
            pageHead
            body $ do
                pageScripts
                header $ h1 "Explore: Apriori"
                div ! A.class_ "container" $ do
                    section ! A.id "config"
                            ! A.class_ "span9" $
                        div ! A.class_ "row" $ do
                            dataConfigHtml
                            filterConfigHtml
                            sortAndGroupConfigHtml
                    hr
                    section ! A.id "apply"
                            ! A.class_ "span9" $
                        button "Apply" ! A.onclick loadButtonClicked
                    section ! A.id "rules"
                            ! A.class_ "span9" $ do
                        h2 "Rules"
                        span "TODO" ! A.class_ "todo"
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

    script "var wSocket = new WebSocket('ws://localhost:9160/'); \
           \wSocket.onclose = function(e) { alert('Connction to server closed!') }"


dataConfigHtml = div ! A.class_ "data col-md-4" $ do
    h3 "Data"
    mkBootstrapButton "Load Data" "btn"
        ! customAttribute "data-toggle" "modal"
        ! customAttribute "data-target" "#upload-data-dialog"
    hr
    span "" ! A.class_ "info"

filterConfigHtml = div ! A.class_ "filter col-md-4" $ do
    h3 "Filters"
    span "TODO" ! A.class_ "todo"

sortAndGroupConfigHtml = div ! A.class_ "sort-group col-md-4" $ do
    h3 "Sort"
    span "TODO" ! A.class_ "todo"
    hr
    h3 "Group"
    span "TODO" ! A.class_ "todo"

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







