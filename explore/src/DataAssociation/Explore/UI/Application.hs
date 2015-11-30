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
, ShowUI(..)

) where

import DataAssociation
import DataAssociation.Explore.Program
import DataAssociation.PostProcess.Descriptor


-----------------------------------------------------------------------------

class ApplicationUITypes a where
    type StatusAppUI
    type RawDataAppUI
    type ConfigAppUI
    type PostFilterAppUI
    type PostSortAppUI
    type PostGroupAppUI
    type ShowAppUI


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

    showStatus :: u -> StatusMessage -> IO ()

-----------------------------------------------------------------------------

class RawDataUI u where
    sendDataDescription :: u -> [(String, String)] -> IO ()

-----------------------------------------------------------------------------

class ShowUI u where
    sendDataToShow :: u set it -> [[AssocRule set it]] -> IO()

-----------------------------------------------------------------------------

