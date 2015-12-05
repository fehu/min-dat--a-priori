{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

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
import DataAssociation.Explore.UI.Web.Application.Message
import DataAssociation.Explore.UI.Web.Render
import DataAssociation.Explore.UI.Web.RulesTransfer
import DataAssociation.Itemset.SetImpl
import WekaData

import Data.IORef
import qualified Data.Set as Set
import Data.List (intercalate)
import qualified Data.Text as T
import Text.JSON

import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import qualified Network.WebSockets as WS

-----------------------------------------------------------------------------

webApp :: (Show WekaVal) => WebApp
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
                    elemHtml $ uiStatus app
                    section ! A.id "config"
                            ! A.class_ "span9" $
                        div ! A.class_ "row" $ do
                            divFor2 "config raw-data-and-conf col-md-3"
                                    "Data"    (uiRawData app)
                                    "Apriori" (uiConfig app)
                            elemHtml $ uiPostFilter app
                            divFor2 "config sort-group col-md-3"
                                    "Sort"  (uiPostSort app)
                                    "Group" (uiPostGroup app)
                    hr
                    section ! A.id "apply"
                            ! A.class_ "span9" $
                        button "Apply" ! A.onclick (stringValue loadButtonClicked)
                    section ! A.id "rules"
                            ! A.class_ "span9" $ do
                        h2 "Rules"
                        elemHtml $ uiShow app
                footer $ do
                    "See project on "
                    a ! A.href "https://github.com/fehu/min-dat--a-priori" $
                        img ! A.src "https://assets-cdn.github.com/images/modules/logos_page/GitHub-Logo.png"
            loadDataDialog
            waitModal "wait-modal"
            constructorDialog
            filterPartSelectorUI


pageHead = head $ do
--    link ! A.rel "stylesheet"
--         ! A.href "https://necolas.github.io/normalize.css/3.0.2/normalize.css"
--
    mkCss "/static/apriori.css"

    mkCss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css"
    mkCss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"

    mkCss "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-switch/3.3.2/css/bootstrap3/bootstrap-switch.min.css"


mkCss url = link ! A.rel "stylesheet"
                 ! A.type_ "text/css"
                 ! A.href url

pageScripts = do
    script "" ! A.src "//code.jquery.com/jquery-2.1.4.min.js"
    script "" ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
    script "" ! A.src "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-switch/3.3.2/js/bootstrap-switch.min.js"

    script "var wSocket = new WebSocket('ws://localhost:9160/');"
    script "" ! A.src "/static/apriori.js"


divFor2 clazz n1 e1 n2 e2 =
    div ! A.class_ (stringValue clazz) $ do
        h3 n1
        elemHtml e1
        h3 n2
        elemHtml e2

loadButtonClicked = sendMessageObjJS [("elem-id", show "update-rules")]

-----------------------------------------------------------------------------

sendMessageObjJS :: [(String, String)] -> String
sendMessageObjJS entries = "sendMessageToServer({" ++ obj ++ "});"
--"wSocket.send(JSON.stringify({" ++ obj ++ "})); waitModal(true);"
    where obj = intercalate ","
              $ Prelude.map (\(k,v) -> "'" ++ k ++ "':" ++ v) entries

elemNameParam' = elemNameParam (undefined :: WebApp)

mkServerMessage elemName param pData = [
      (elemNameParam', show elemName)
    , (param         , pData)
    ]

-----------------------------------------------------------------------------

someModal id =  div ! A.id id
                 ! A.class_ "modal fade"
                 ! customAttribute "role" "dialog"
                 ! customAttribute "aria-hidden" "true"

waitModal id = someModal id . div
             $ div ! A.class_ "modal-dialog"
             $ i "" ! A.class_ "glyphicon glyphicon-refresh glyphicon-spin"

-----------------------------------------------------------------------------

loadDataDialog = someModal "upload-data-dialog"
                            ! A.tabindex "-1"
                            ! customAttribute "aria-labelledby" "upload-data-dialog-label"
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
                                ! A.onclick (stringValue . sendMessageObjJS $
                                             mkServerMessage "raw-data"
                                                             "raw-data"
                                                             "$('#upload-data-dialog-text').val()"
                                            )
                            mkBootstrapCloseModalButton "Cancel" "btn btn-inverse"

-----------------------------------------------------------------------------

mkBootstrapButton txt clazz = button txt ! A.type_ "button"
                                         ! A.class_ clazz

mkBootstrapCloseModalButton txt clazz =
    mkBootstrapButton txt clazz
        ! customAttribute "data-dismiss" "modal"
        ! customAttribute "aria-hidden" "true"

-----------------------------------------------------------------------------

statusList = StatusList{
    statusShow = msg2UI
  , statusHtml = div "" ! A.id "statuses"
}

-----------------------------------------------------------------------------

rawDataTextAreaDialog = RawDataTextAreaDialog{
    rawDataSendDescr = \r -> msg2UI r . dataUpdateMsg

  , rawDataHtml = do
        mkBootstrapButton "Load Data" "btn btn-default"
            ! customAttribute "data-toggle" "modal"
            ! customAttribute "data-target" "#upload-data-dialog"
        hr
        let space = preEscapedToHtml ("&nbsp;" :: String)
        dl ! A.class_ "info" $ do
            dt "Name:"
            dd space ! A.id "data-name"
            dt "Attributes:"
            dd space ! A.id "data-attrs"
            dt "Entries:"
            dd space ! A.id "data-count"
}

-----------------------------------------------------------------------------

mkNumberInput labelTxt id = do
    label labelTxt ! A.for id
    input ! A.type_ "number"
          ! A.id id
          ! A.name id
          ! A.value "0.0"
          ! A.step "any" -- 0.01
          ! A.min "0.0"
          ! A.max "1.0"
          ! A.required "true"

inputChangeJS name param =
    "f = function(e){\
    \ if(e.currentTarget.validity.valid) {"
    ++ sendMessageObjJS (mkServerMessage name param "e.currentTarget.value")
    ++ "  }; };\
    \$('#" ++ name ++ "').change(f)"

aprioriConfigUI = AprioriConfigUI ( aprioriConfigMSupUI
                                  , aprioriConfigMConfUI
                                  , form ! A.id "apriori-form" $
                                        do elemHtml aprioriConfigMSupUI
                                           elemHtml aprioriConfigMConfUI
                                  )

aprioriConfigMSupUI = AprioriConfigMSupUI $ do
    mkNumberInput "Min. Support" "min-support"
    script . string $ inputChangeJS "min-support" "apriori-param"

aprioriConfigMConfUI = AprioriConfigMConfUI $ do
    mkNumberInput "Min. Confidence" "min-confidence"
    script . string $ inputChangeJS "min-confidence" "apriori-param"

-----------------------------------------------------------------------------

postProcessFilterBuilderUI = PostProcessFilterBuilderUI $
    div ! A.class_ "config filter col-md-4" $
        do h3 "Filters"
           div ! A.class_ "btn-group"
               $ mkBootstrapButton "Add" "btn btn-success"
           div "" ! A.id "filters-list"

filterPartSelectorMenu = ul ! A.class_ "dropdown-menu"
                            ! customAttribute "role" "menu"
--                            ! customAttribute "aria-labelledby"
--                                              "filter-part-dropdown"
                            $
                                do li $ a "Contains"        ! A.class_ "create-contains"
                                   li $ a "Has Attribute"   ! A.class_ "create-has"
                                   li $ a "Not"             ! A.class_ "create-not"
                                   li $ a "And"             ! A.class_ "create-and"
                                   li $ a "Or"              ! A.class_ "create-or"

filterPartSelDropdown= div ! A.class_ "dropdown"
                       $ do
                            a ! A.class_ "dropdown-toggle"
--                              ! A.id "filter-part-dropdown"
                              ! customAttribute "role" "button"
                              ! customAttribute "data-toggle" "dropdown"
                              ! customAttribute "data-target" "#"
                              $ i "" ! A.class_ "glyphicon glyphicon-plus"
                            filterPartSelectorMenu

filterPartSelectorUI = div ! A.id "filter-part-menu"
                           ! A.class_ "create-filter-part"
                           ! A.hidden "true"
                           $ filterPartSelDropdown

postProcessSortBuilderUI = PostProcessSortBuilderUI $
    span "TODO" ! A.class_ "todo"

postProcessGroupBuilderUI = PostProcessGroupBuilderUI $
    span "TODO" ! A.class_ "todo"

-----------------------------------------------------------------------------

showProcessedDataUI :: (Show WekaVal) => ShowProcessedDataUI Set Item
showProcessedDataUI = ShowProcessedDataUI{
    sendDataToUI = \r -> msg2UI r . rulesUpdateMsg
  , showDataHtml = div "" ! A.class_ "rules-groups"
}

-----------------------------------------------------------------------------

selectSideRadio = do span "Rule side: "
                     div  ! A.class_ "rule-side btn-group"
                          ! dataAttribute "toggle" "buttons"
                          $ do
                                mkRadio "Left"
                                mkRadio "Right"

mkRadio txt = label ! A.class_ "btn btn-primary" $ do
                  input ! A.type_ "radio"
                        ! A.value (stringValue txt)
                        ! A.autocomplete "off"
                  string txt

constructorDialog = someModal "constructor-dialog"
                   $ div ! A.class_ "modal-dialog"
                   $ div ! A.class_ "modal-content"
                   $ do div ! A.class_ "modal-header" $ do
                            mkBootstrapCloseModalButton "×" "close"
                            selectSideRadio
                        div "" ! A.class_ "contents"
                        div ! A.class_ "modal-footer" $ do
                            mkBootstrapCloseModalButton "Apply" "btn btn-primary"
                                ! A.class_ "submit"
                            mkBootstrapCloseModalButton "Cancel" "btn btn-inverse"



