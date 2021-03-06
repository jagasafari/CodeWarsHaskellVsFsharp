{-# LANGUAGE OverloadedStrings #-}
module ScottyActions where

import Web.Scotty
import Data.Monoid 
import ExploringIO
import Control.Monad
import Serializings
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Data.Text.Lazy (pack)

existsDirContent = do 
    searchedContent <- param "searchedContent"
    fileExist <- liftIO $ doesFileExistCurrentDir searchedContent 
    text $ pack $ show fileExist

currentDir = do 
    content <- liftIO currentDirContentFiltered 
    json content

ioworkflow = do
    workflow <- liftIO runWorkflow
    text $ mconcat $ map pack workflow

getUsers = do
   json $ nUsers 6 

getUsersId = do
    id <- param "id"
    json $ (filter (\user -> userId user < id) $ nUsers 69)

displayStaticHtmlFile = do
    setHeader "Content-Type" "text/html"
    file "src/UrlToMatch.html"

monkeyUrlTextInput = do
    name <- param "name"
    text $ mconcat ["monkey input: ", name]

jumpToNextAction = do
    enteredUrl <- param "next"
    when (enteredUrl == "next") next
    text $ mconcat ["entered url : /", enteredUrl]

regexCapture = do
    path <- param "0"
    capture <- param "1"
    html $ mconcat ["<h1>", "path: ", path, "capture: ", capture, "</h1>"]

twoInputs = do
    appendValue <- param "append"
    removeValue <- param "remove"
    html $ mconcat ["<h6>", "append: ", appendValue, " and remove: ", removeValue, "</h6>"]

redirectToGoogle = do
    redirect "http://www.google.com"

simulateErrorEndRescue = do
    raise "simulate error" `rescue` \er -> text er
