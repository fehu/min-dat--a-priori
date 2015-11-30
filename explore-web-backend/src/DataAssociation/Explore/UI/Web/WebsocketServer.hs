{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.UI.Web.WebsocketServer
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

module DataAssociation.Explore.UI.Web.WebsocketServer (

  wsserver

) where

import DataAssociation
import DataAssociation.Explore.UI.Web.Application
import DataAssociation.Explore.UI.State
import WekaData

import qualified Network.WebSockets as WS

import Control.Concurrent
import Control.Monad (forever)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import Text.JSON
import Text.JSON.String


wsserver :: (ReactiveWebElemSelector app (AprioriWebAppState cache)) =>
          app
       -> InitialState cache RawWekaData (MinSupport, MinConfidence)
       -> WS.ServerApp
wsserver app iState pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    state <- newStateWithInitial iState
    putStrLn "created new state"

    forever $ do
        msg   <- WS.receiveData conn
        putStrLn $ "processing message: " ++ T.unpack msg

        let Right (JSObject obj) = runGetJSON readJSObject (T.unpack msg)
        let jObj = fromJSObject obj
        let Just (JSString eId) = lookup (elemNameParam app state) jObj

        case reactiveWebElemByName app state $ fromJSString eId
            of SomeReactiveWebElem e -> reqParse e jObj state

        putStrLn "done"



