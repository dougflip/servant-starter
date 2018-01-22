{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

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

getInt :: String -> Int
getInt = read

data StarWarsPerson = StarWarsPerson
  { name      :: String
  , height    :: Int
  , birthYear :: String
  } deriving (Show, Generic)

-- This works, but I wonder if there is a way to "auto" map the easy fields
-- and specify just the ones that need a mapping...
instance FromJSON StarWarsPerson where
  parseJSON =
    withObject "StarWarsPerson" $ \v ->
      StarWarsPerson <$> v .: "name" <*> (getInt <$> v .: "height") <*>
      v .: "birth_year"

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
