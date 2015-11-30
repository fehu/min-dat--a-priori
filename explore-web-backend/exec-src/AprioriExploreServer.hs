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
-----------------------------------------------------------------------------

module Main ( main ) where


import DataAssociation
import DataAssociation.Explore.UI.Web.Server
import DataAssociation.Explore.UI.Web.Application
import DataAssociation.Explore.UI.State
import qualified DataAssociation.Explore.UI.Web.Application.DefaultImpl as Impl
import WekaData

import qualified Network.WebSockets as WS


main = do
    app <- Impl.webApp
    let iState = InitialState undefined (RawWekaData "" [] []) (MinSupport 0, MinConfidence 0)

    WS.runServer "0.0.0.0" 9160 $ server app iState
