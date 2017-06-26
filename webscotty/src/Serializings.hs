{-# LANGUAGE DeriveGeneric #-}
module Serializings where

import System.Random
import Data.Aeson
import GHC.Generics

data User = User { userId :: Int, name :: String } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

getRandomX :: (Int,Int) -> Int
getRandomX (x,y) = r where (r:rs) = randomRs (x,y) $ mkStdGen 77
nBobUsers n = replicate n $ User { userId = 7777777, name = "bob" }
nUsers n = map (\x -> User { userId = x, name = "bob" }) $ take n $ randomRs (1, 100) (mkStdGen 876)
