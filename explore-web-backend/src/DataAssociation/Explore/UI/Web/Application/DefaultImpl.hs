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

import DataAssociation
import DataAssociation.Explore.UI.Web.Application

import Data.IORef
import qualified Data.Set as Set



webApp = do
    rawDataUI  <- rawDataTextAreaDialog
    aprioriUI  <- aprioriConfigUI
    ppFilterUI <- postProcessFilterBuilderUI
    ppSortUI   <- postProcessSortBuilderUI
    ppGroupUI  <- postProcessGroupBuilderUI

    let showUI = showProcessedDataUI
    let statUI = statusList

    return $ WebApp rawDataUI
                    aprioriUI
                    ppFilterUI
                    ppSortUI
                    ppGroupUI
                    showUI
                    statUI



statusList = StatusList{
    statusShow = undefined
  , statusHtml = undefined
}

rawDataTextAreaDialog = do
    rdRef <- newIORef []

    return RawDataTextAreaDialog{
        rawDataReqParam  = "raw-data"
      , rawDataReqPath   = ["load-raw-data"]
      , rawDataSendDescr = undefined
      , rawDataHtml      = undefined
    }

aprioriConfigUI = do
    msRef <- newIORef (MinSupport 0)
    mcRef <- newIORef (MinConfidence 0)

    return AprioriConfigUI {
--        aprioriMinSupRef     = msRef
--      , aprioriMinConfRef    = mcRef
        apriofiConfigReqPath = ["set-apriori-params"]
      , aprioryConfigHtml = undefined
    }


ppPath = "set-post-process"


postProcessFilterBuilderUI = do
    ref <- newIORef Set.empty

    return PostProcessFilterBuilderUI{
--        ppFilterRef     = ref
        ppFilterReqPath = [ppPath, "filter"]
      , ppFilterHtml = undefined
    }

postProcessSortBuilderUI = do
    ref <- newIORef []

    return PostProcessSortBuilderUI{
--        ppSortRef     = ref
        ppSortReqPath = [ppPath, "sort"]
      , ppSortHtml = undefined
    }

postProcessGroupBuilderUI = do
    ref <- newIORef Nothing

    return PostProcessGroupBuilderUI{
--        ppGroupRef     = ref
        ppGroupReqPath = [ppPath, "group"]
      , ppGroupHtml = undefined
    }

showProcessedDataUI = ShowProcessedDataUI{
    sendDataToUI = undefined,
    showDataHtml = undefined
}







