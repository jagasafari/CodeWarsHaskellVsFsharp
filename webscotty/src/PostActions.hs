{-# LANGUAGE OverloadedStrings #-}
module PostActions where

import qualified Web.Scotty as WS 
import Network.HTTP.Types.Status
import Data.Aeson

simulateFailure :: WS.ActionM ()
simulateFailure = do
    name <- WS.param "name" `WS.rescue` (const WS.next)
    WS.text name

handleSimulatedFailure :: WS.ActionM ()
handleSimulatedFailure = do
    WS.json $ object [ "error" .= ("Invalid request"::String) ]
    WS.status badRequest400

deserializingJsonData = do 
    [dat, sec] <- WS.jsonData :: WS.ActionM [String]
    WS.status status204
