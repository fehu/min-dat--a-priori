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

import DataAssociation.Explore.UI.Web.Application
import DataAssociation.Explore.UI.State
import DataAssociation.Explore.UI.Web.Render

import Web.Spock

import Text.Blaze.Html.Renderer.Text
import Data.List (intercalate)
import qualified Data.Text as T

import qualified Data.Text.Lazy as ST
import Control.Monad.IO.Class

import System.FilePath

listenToReactiveElems :: (MonadIO m) => [SomeRenderableWebPage] -> SpockT m ()
listenToReactiveElems  elems = sequence_ handlers
    where handlers = do SomeRenderableWebPage e <- elems
                        return $ get (static . intercalate "/" $ reqPath e)
                                     (html . ST.toStrict . renderHtml $ renderWebPage e)

server :: Int -> [SomeRenderableWebPage] -> [FilePath] -> IO ()
server port pages staticRoot = runSpock port . spockT id $ do
    listenToReactiveElems pages

    let getResource path name = get path $ file name $ joinPath (staticRoot ++ [T.unpack name])

    getResource "static/apriori.css" "apriori.css"
    getResource "static/apriori.js"  "apriori.js"
