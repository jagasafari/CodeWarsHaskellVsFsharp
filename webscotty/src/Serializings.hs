{-# LANGUAGE DeriveGeneric #-}
module Serializings where

import Data.Aeson
import GHC.Generics

data User = User { userId :: Int, name :: String } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

nBobUsers n = replicate n $ User { userId = 98, name = "bob" }

