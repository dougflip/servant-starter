{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( module Lib -- TODO: Exporting everything to make ghci easier, be specific after things are working
  ) where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data StarWarsPerson = StarWarsPerson
  { name :: String
  } deriving (Show, Generic)

instance FromJSON StarWarsPerson

type StarWarsAPI
   = "api" :> "people" :> Capture "userid" Int :> Get '[ JSON] StarWarsPerson

apiInstance :: Proxy StarWarsAPI
apiInstance = Proxy

baseApiUrl :: String -> BaseUrl
baseApiUrl = BaseUrl Http "swapi.co" 80

getStarWarsPerson :: Int -> ClientM StarWarsPerson
getStarWarsPerson = client apiInstance

run :: IO ()
run = do
  manager' <- newManager tlsManagerSettings
  res <-
    runClientM
      (getStarWarsPerson 1)
      (ClientEnv manager' (BaseUrl Https "swapi.co" 443 ""))
  case res of
    Left err     -> putStrLn $ "Oh no! Error: " ++ show err
    Right person -> print person
