{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
    runServer,
  )
where

import Data.Aeson
import Data.Monoid (mconcat)
import GHC.Generics
import Web.Scotty as SC

data User = User {uid :: Integer, name :: String, age :: Integer} deriving (Generic, Show)

instance ToJSON User

instance FromJSON User

runServer :: IO ()
runServer = scotty 3000 $ do
  get "/" $ do
    SC.json $ User {uid = 1, name = "name", age = 1}
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
