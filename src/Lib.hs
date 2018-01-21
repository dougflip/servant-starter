{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( someFunc
    , getStarWarsPerson
    ) where

import Data.Aeson
import GHC.Generics
import Data.Proxy
import Servant.API
import Servant.Client

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data StarWarsPerson = StarWarsPerson
    { name :: String } deriving (Show, Generic)

instance FromJSON StarWarsPerson

type StarWarsAPI = "api" :> "people" :> Capture "userid" Int :> Get '[JSON] StarWarsPerson

apiInstance :: Proxy StarWarsAPI
apiInstance = Proxy

getStarWarsPerson = client apiInstance
