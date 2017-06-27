{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , module Serializings
    ) where
import Serializings
import Web.Scotty
import qualified Data.Text.Lazy as TL (pack)
import qualified Network.Wai as NW
import ScottyActions
import PostActions
routes = do
    post "/simulateFailure" simulateFailure
    post "/simulateFailure" handleSimulatedFailure
    post "/deserializingJsonData" deserializingJsonData 
    get "/" displayStaticHtmlFile
    get "/getUsers" getUsers
    get "/currentDir" currentDir
    get "/existsDirContent/:searchedContent" existsDirContent
    get "/text/:name" monkeyUrlTextInput
    get "/getUsers/:id" getUsersId
    get "/:next" jumpToNextAction
    get (regex "^/f(.*)r$") regexCapture
    get "/:append/:remove" twoInputs
    get "/redirect" redirectToGoogle
    get "/rescue" simulateErrorEndRescue
    get (function $ \req -> Just [("version", TL.pack $ show $ NW.httpVersion req)]) $ do
        v <- param "version"
        text v
someFunc :: IO ()
someFunc = scotty 3000 $ routes
