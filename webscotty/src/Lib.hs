{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where
import qualified Web.Scotty as WS 
import qualified Data.Text.Lazy as TL (pack)
import qualified Network.Wai as NW
import ScottyActions
import PostActions

someFunc :: IO ()
someFunc = WS.scotty 3000 $ do
    WS.post "/simulateFailure" simulateFailure
    WS.post "/simulateFailure" handleSimulatedFailure
    WS.get "/" displayStaticHtmlFile
    WS.get "/text/:name" monkeyUrlTextInput
    WS.get "/:next" jumpToNextAction
    WS.get (WS.regex "^/f(.*)r$") regexCapture
    WS.get "/:append/:remove" twoInputs
    WS.get "/redirect" redirectToGoogle
    WS.get "/rescue" simulateErrorEndRescue
    WS.get (WS.function $ \req -> Just [("version", TL.pack $ show $ NW.httpVersion req)]) $ do
        v <- WS.param "version"
        WS.text v
