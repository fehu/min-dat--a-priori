{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}

-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.UI.Application
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

module DataAssociation.Explore.UI.Application(

  UIApplication(..)
, UIApplicationTypes(..)

, RawDataUI(..)
, PostProcessUI(..)
, ShowUI(..)

) where

import DataAssociation
import DataAssociation.PostProcess.Descriptor


class UIApplicationTypes a where
    type RawDataAppUI
    type ConfigAppUI
    type PostFilterAppUI
    type PostSortAppUI
    type PostGroupAppUI
    type ShowAppUI


class UIApplication a where

    uiRawData    :: a -> RawDataAppUI
    uiConfig     :: a -> ConfigAppUI
    uiPostFilter :: a -> PostFilterAppUI
    uiPostSort   :: a -> PostSortAppUI
    uiPostGroup  :: a -> PostGroupAppUI
    uiShow       :: a -> ShowAppUI


--data RawDataAppUI = forall u . RawDataUI u => RawDataAppUI u

class RawDataUI u where
    type RawData

    getRawData :: u -> IO RawData
    setRawData :: u -> RawData -> IO()


class PostProcessUI u d where
    getPostProcess :: u -> IO [d]
    setPostProcess :: u -> [d] -> IO()


class ShowUI u where
    sendDataToShow :: u set it -> [[AssocRule set it]] -> IO()


