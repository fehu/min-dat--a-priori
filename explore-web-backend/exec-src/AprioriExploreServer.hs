{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module Main ( main ) where


import DataAssociation
import DataAssociation.Itemset.SetImpl
import DataAssociation.SimpleRulesGenerator

import DataAssociation.Explore.UI.Web.Server
import DataAssociation.Explore.UI.Web.WebsocketServer
import DataAssociation.Explore.UI.Web.Application
import DataAssociation.Explore.UI.Web.Render
import DataAssociation.Explore.UI.State
import qualified DataAssociation.Explore.UI.Web.Application.DefaultImpl as Impl
import WekaData
import WekaData.Show.Full

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent (forkIO)
import GHC.IO.Handle
import qualified GHC.IO.Handle.FD as FD

import qualified Network.WebSockets as WS

-----------------------------------------------------------------------------

instance WekaEntryToItemset Set Item where
    wekaEntryToItemset (WEntry vset) = Set.map f vset
        where f (WVal (WekaAttrNom a [_]) _) = Item a
              f (WVal (WekaAttrNom _  _ ) v) = Item v
              f (WVal (WekaAttrNum _)     _) = error "The data is expected to be nominal"

-----------------------------------------------------------------------------

main = do
    let app = Impl.webApp
    let iState = InitialState (error "no data to analyse has been uploaded"
                                            :: AprioriWebAppCache Set Item)
                              (RawWekaData "" [] [])
                              (MinSupport 0, MinConfidence 0)

    forkIO $ server 8080 [SomeRenderableWebPage app] ["static"]
    forkIO $ WS.runServer "0.0.0.0" 9160 $ wsserver app iState

    putStrLn "Server is running, press ENTER to stop"
    hWaitForInput FD.stdin (-1)
