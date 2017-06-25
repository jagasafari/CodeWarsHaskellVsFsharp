{-# LANGUAGE OverloadedStrings #-}
module ScottyActions where

import qualified Web.Scotty as WS 
import Data.Monoid 
import Control.Monad
import Serializings

getUsers = do
   WS.json $ nBobUsers 6 
displayStaticHtmlFile = do
    WS.setHeader "Content-Type" "text/html"
    WS.file "src/UrlToMatch.html"

monkeyUrlTextInput = do
    name <- WS.param "name"
    WS.text $ mconcat ["monkey input: ", name]

jumpToNextAction = do
    enteredUrl <- WS.param "next"
    when (enteredUrl == "next") WS.next
    WS.text $ mconcat ["entered url : /", enteredUrl]

regexCapture = do
    path <- WS.param "0"
    capture <- WS.param "1"
    WS.html $ mconcat ["<h1>", "path: ", path, "capture: ", capture, "</h1>"]

twoInputs = do
    appendValue <- WS.param "append"
    removeValue <- WS.param "remove"
    WS.html $ mconcat ["<h6>", "append: ", appendValue, " and remove: ", removeValue, "</h6>"]

redirectToGoogle = do
    WS.redirect "http://www.google.com"

simulateErrorEndRescue = do
    WS.raise "simulate error" `WS.rescue` \er -> WS.text er

