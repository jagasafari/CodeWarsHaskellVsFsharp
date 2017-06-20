{-# LANGUAGE OverloadedStrings #-}
module PostActions where

import qualified Web.Scotty as WS 
import Network.HTTP.Types.Status
import Data.Aeson

simulateFailure :: WS.ActionM ()
simulateFailure = do
    error "simulated error"

handleSimulatedFailure :: WS.ActionM ()
handleSimulatedFailure = do
    WS.json $ object [ "error" .= ("Invalid request"::String) ]
    WS.status badRequest400
