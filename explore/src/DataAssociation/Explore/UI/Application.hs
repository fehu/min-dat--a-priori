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

module DataAssociation.Explore.UI.Application(

  ApplicationUI(..)
, ApplicationUITypes(..)

, StatusUI(..)
, RawDataUI(..)
, RawDataInnerRepr(..)
, PostProcessInnerRepr(..)
, ShowUI(..)

) where

import DataAssociation
import DataAssociation.Explore.Program
import DataAssociation.PostProcess


-----------------------------------------------------------------------------

class ApplicationUITypes a where
    type StatusAppUI
    type RawDataAppUI
    type ConfigAppUI
    type PostFilterAppUI
    type PostSortAppUI
    type PostGroupAppUI
    type ShowAppUI

    type MessagingContext


class ApplicationUI a where

    uiStatus     :: a -> StatusAppUI
    uiRawData    :: a -> RawDataAppUI
    uiConfig     :: a -> ConfigAppUI
    uiPostFilter :: a -> PostFilterAppUI
    uiPostSort   :: a -> PostSortAppUI
    uiPostGroup  :: a -> PostGroupAppUI
    uiShow       :: a -> ShowAppUI


-----------------------------------------------------------------------------

class StatusUI u where
    type StatusMessage

    showStatus :: u -> MessagingContext -> StatusMessage -> IO ()

-----------------------------------------------------------------------------

class RawDataUI u where
    type RawDataDescription
    sendDataDescription :: u -> MessagingContext -> RawDataDescription -> IO ()

class RawDataInnerRepr r where
    type RawData

    getRawData :: r -> IO RawData
    setRawData :: r -> RawData -> IO()

-----------------------------------------------------------------------------

class PostProcessInnerRepr u d where
    getPostProcess :: u -> IO [d]
    setPostProcess :: u -> [d] -> IO()

-----------------------------------------------------------------------------

class ShowUI u where
    sendDataToShow :: u set it -> MessagingContext -> [[AssocRule set it]] -> IO()

-----------------------------------------------------------------------------

