{-# LANGUAGE OverloadedStrings #-}
module PostActions where

import qualified Web.Scotty as WS 
import Data.Aeson
import Network.HTTP.Types.Status
import Serializings

deserializingJsonData = do 
    user <- WS.jsonData :: WS.ActionM User
    WS.status status204

simulateFailure :: WS.ActionM ()
simulateFailure = do
    name <- WS.param "name" `WS.rescue` (const WS.next)
    WS.text name

handleSimulatedFailure :: WS.ActionM ()
handleSimulatedFailure = do
    WS.json $ object [ "error" .= ("Invalid request"::String) ]
    WS.status badRequest400
