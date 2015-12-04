{-# LANGUAGE -- TypeFamilies
--           , FlexibleInstances
             ExistentialQuantification
--           , OverloadedStrings
--           , FlexibleContexts
         #-}

-----------------------------------------------------------------------------
--
-- Module      :  DataAssociation.Explore.UI.Web.Application.Message
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module DataAssociation.Explore.UI.Web.Application.Message (

  WebAppMsg(..)
, SomeMsg(..)
, Message2UI(..)
, msg2UI

, DoneMsg(..)

, WebAppStatusMsg(..)
, statusUpdMsg
, statusErrMsg

, WebAppDataInfoMsg(..)
, dataUpdateMsg

, WebAppDataRulesMsg(..)
, rulesUpdateMsg

) where

import DataAssociation
import DataAssociation.Explore.UI.Web.RulesTransfer
import WekaData

import Text.JSON

-----------------------------------------------------------------------------

class WebAppMsg msg where messageType   :: msg -> String
                          messageToJson :: msg -> JSValue

data SomeMsg = forall msg . WebAppMsg msg => SomeMsg msg
instance WebAppMsg SomeMsg where messageType (SomeMsg msg) = messageType msg
                                 messageToJson (SomeMsg msg) = messageToJson msg

data Message2UI = Message2UI (SomeMsg -> IO ())

msg2UI :: (WebAppMsg msg) => Message2UI -> msg -> IO ()
msg2UI (Message2UI send) = send . SomeMsg

-----------------------------------------------------------------------------

data DoneMsg = DoneMsg

instance WebAppMsg DoneMsg where messageType   _ = "done"
                                 messageToJson _ = makeObj [("type", showJSON "done")]

-----------------------------------------------------------------------------

data StatusType = StatusOk | StatusError

instance Show StatusType where show StatusOk    = "status"
                               show StatusError = "error"

data WebAppStatusMsg = WebAppStatusMsg{
    statusValue      :: JSValue
  , statusType       :: StatusType
  , statusShowMillis :: Maybe Int
  , statusPriority   :: Int
}

instance WebAppMsg WebAppStatusMsg where messageType = show . statusType
                                         messageToJson = statusMessageToJson

statusUpdMsg :: String -> WebAppStatusMsg
statusUpdMsg msg = WebAppStatusMsg (showJSON msg) StatusOk (Just 5000) 1

statusErrMsg :: String -> WebAppStatusMsg
statusErrMsg msg = WebAppStatusMsg (showJSON msg) StatusError Nothing 99

statusMessageToJson (WebAppStatusMsg msg tpe millis priority) = JSObject $ toJSObject [
      ("type",       showJSON . show $ tpe)
    , ("message",    msg)
    , ("showMillis", showJSON millis)
    , ("priority",   showJSON priority)
    ]

-----------------------------------------------------------------------------

newtype WebAppDataInfoMsg = WebAppDataInfoMsg JSValue

instance WebAppMsg WebAppDataInfoMsg where messageType = const "data-info"
                                           messageToJson (WebAppDataInfoMsg j) = j

dataUpdateMsg :: RawWekaData -> WebAppDataInfoMsg
dataUpdateMsg wData = WebAppDataInfoMsg . JSObject $ toJSObject [
              ("type",  showJSON $ messageType (undefined :: WebAppDataInfoMsg))
            , ("name",  showJSON $ rwdName wData)
            , ("attrs", showJSON . length $ rwdAttrs wData)
            , ("count", showJSON . length $ rawWekaData wData)
            ]


-----------------------------------------------------------------------------

newtype WebAppDataRulesMsg set it = WebAppDataRulesMsg (GroupedRules set it)

instance (Itemset set it) =>
    WebAppMsg (WebAppDataRulesMsg set it) where
        messageType = const "rules"
        messageToJson (WebAppDataRulesMsg msg) = makeObj [
            ("type", showJSON "rules")
          , ("rules", groupedRules2JSON msg)
         ]

rulesUpdateMsg :: GroupedRules set it -> WebAppDataRulesMsg set it
rulesUpdateMsg = WebAppDataRulesMsg

-----------------------------------------------------------------------------



