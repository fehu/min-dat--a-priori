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
import Data.HVect

import Data.List (intercalate)
import qualified Data.Text as T

import Control.Arrow
import Control.Exception
import Control.Monad.IO.Class




listenToReactiveElems :: [SomeReactiveWebElem state]
                      -> SpockT m ()
listenToReactiveElems  elems =undefined --  sequence_ handlers
    where handlers = do SomeReactiveWebElem e <- elems
                        return $ do mbP <- param . T.pack $ reqParam e
                                    return  ( static . intercalate "/" $ reqPath e
                                            , modifySession $ \s -> reqParse e mbP (s :: state)
                                            )

--listenToReactiveElems elems = sequence_ handlers
--    where handlers = do SomeReactiveWebElem e <- elems
--                        return $ do mbP <- param . T.pack $ reqParam e
--                                    s   <- readSession
--                                    return $ post (static . intercalate "/" $ reqPath e)
--                                                  (return $ reqParse e s mbP)

--                         post (static . intercalate "/" $ reqPath e)
--                                            (reqParse e s )


--listenTo :: ReactiveWebElem u => u -> ActionCtxT ctx IO a
--listenTo u = post (static . intercalate "/" $ reqPath u) $ do
--    let f = reqParse u
--    p <- param . T.pack $ reqParam u

--    let res = fmap (const "ok") (f p) `catch` (\e -> return $ show (e :: SomeException))



--    text ""

--    return $ fmap (text . T.pack) res

server app = spockT id $ do

    let rawData = uiRawData app

    post (static $ intercalate "/" . rawDataReqPath $ rawData) $ do
--        sentT <- param' "raw-data"
--        let wData = map T.unpack $ T.splitOn "\n" sentT
--        return $ setRawData rawData wData

        text "ok"

--    let rawData         = (rawDataReqPath &&& rawDataHtml) . uiRawData
--    let aprioriConfig   = (apriofiConfigReqPath &&& aprioryConfigHtml). uiConfig
--    let postFilter      = (uiPostFilter

    -- intercalate "/" .

--    get (static rawDataPath) $ lazyBytes . renderHtml . rawDataHtml $ uiRawData app
--        text $ T.concat ["requested ", "rawDataReqPath"]

