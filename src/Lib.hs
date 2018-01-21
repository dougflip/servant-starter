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

data StarWarsPerson = StarWarsPerson
  { name :: String
  } deriving (Show, Generic)

instance FromJSON StarWarsPerson

type StarWarsAPI
   = "people" :> Capture "userid" Int :> Get '[ JSON] StarWarsPerson

apiInstance :: Proxy StarWarsAPI
apiInstance = Proxy

baseApiUrl :: BaseUrl
baseApiUrl = BaseUrl Https "swapi.co" 443 "/api"

getStarWarsPerson :: Int -> ClientM StarWarsPerson
getStarWarsPerson = client apiInstance

run :: Int -> IO ()
run id = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM (getStarWarsPerson id) (ClientEnv manager' baseApiUrl)
  case res of
    Left err     -> putStrLn $ "Oh no! Error: " ++ show err
    Right person -> print person
