{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.UI.Web.Server
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module DataAssociation.Explore.UI.Web.Server (

  server

) where

import DataAssociation.Explore.UI.Application
import DataAssociation.Explore.UI.Web.Application


import Web.Spock
import Text.Blaze.Html.Renderer.Utf8

import Data.List (intercalate)
import qualified Data.Text as T

import Control.Arrow




server app = spockT id $ do

    let rawData = uiRawData app

    post (static $ intercalate "/" . rawDataReqPath $ rawData) $ do
        sentT <- param' "raw-data"
        let wData = map T.unpack $ T.splitOn "\n" sentT
        return $ setRawData rawData wData

        text "ok"

--    let rawData         = (rawDataReqPath &&& rawDataHtml) . uiRawData
--    let aprioriConfig   = (apriofiConfigReqPath &&& aprioryConfigHtml). uiConfig
--    let postFilter      = (uiPostFilter

    -- intercalate "/" .

--    get (static rawDataPath) $ lazyBytes . renderHtml . rawDataHtml $ uiRawData app
--        text $ T.concat ["requested ", "rawDataReqPath"]

