{-# LANGUAGE ExistentialQuantification #-}

-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.UI.Web.Render
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module DataAssociation.Explore.UI.Web.Render (

  ReqPath

, RenderableWebPage(..)
, SomeRenderableWebPage(..)

) where

import Text.Blaze.Html5 (Html)

-----------------------------------------------------------------------------

type ReqPath = [String]

class RenderableWebPage u where
    reqPath    :: u -> ReqPath
    renderWebPage :: u -> Html


data SomeRenderableWebPage = forall u . RenderableWebPage u =>
     SomeRenderableWebPage u


