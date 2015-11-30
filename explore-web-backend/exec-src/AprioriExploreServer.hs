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


import DataAssociation.Explore.UI.Web.Server
import DataAssociation.Explore.UI.Web.Application.DefaultImpl

import Web.Spock


main = do
    app <- webApp
    runSpock 8080 $ server app

