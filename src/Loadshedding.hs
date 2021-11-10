{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Loadshedding
    ( newLoadsheddingClient
    , LoadsheddingClient
    , LoadsheddingStatus
    , getLoadsheddingStatus
    , getMunicipalities
    , Municipality (..)
    ) where

import GHC.Generics
import Data.Aeson
import Data.Proxy
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

newtype LoadsheddingClient = LoadsheddingClient { _env :: ClientEnv }

type LoadsheddingStatus = Int

data MunicipalityRaw = MunicipalityRaw
  { rawName  :: String
  , rawValue :: String
  } deriving (Eq, Show, Generic)

instance FromJSON MunicipalityRaw where
  parseJSON (Object o) =
    MunicipalityRaw <$> o .: "Text"
                    <*> o .: "Value"
  parseJSON _ = undefined

data Municipality = Municipality
  { municipalityName  :: String
  , municipalityId    :: Int
  } deriving (Eq, Show)

type LoadsheddingAPI =
        "getstatus" :> Get '[JSON] LoadsheddingStatus
  :<|>  "getmunicipalities" :> QueryParam "id" Int :> Get '[JSON] [MunicipalityRaw]

loadsheddingAPI :: Proxy LoadsheddingAPI
loadsheddingAPI = Proxy

processMunicipality :: MunicipalityRaw -> Municipality
processMunicipality raw = Municipality
  { municipalityName  = rawName raw
  , municipalityId    = read $ rawValue raw
  }

newLoadsheddingClient :: IO LoadsheddingClient
newLoadsheddingClient = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "loadshedding.eskom.co.za" 443 "loadshedding")
  return $ LoadsheddingClient env

queryLoadshedding :: LoadsheddingClient -> ClientM a -> IO (Either ClientError a)
queryLoadshedding client query = runClientM query env
  where env = _env client

getLoadsheddingStatusRaw  :: ClientM LoadsheddingStatus
getMunicipalitiesRaw      :: Maybe Int -> ClientM [MunicipalityRaw]
getLoadsheddingStatusRaw :<|> getMunicipalitiesRaw = client loadsheddingAPI

-- The API returns 2 for stage 1 etc
-- This returns 0 for no load shedding, 1 for stage 1 etc
getLoadsheddingStatus :: LoadsheddingClient -> IO (Either ClientError LoadsheddingStatus)
getLoadsheddingStatus client = do
  res <- queryLoadshedding client getLoadsheddingStatusRaw
  case res of
    Left err  -> return $ Left err
    Right val -> return $ Right $ val - 1

getMunicipalities :: LoadsheddingClient -> Int -> IO (Either ClientError [Municipality])
getMunicipalities client p = do
  res <- queryLoadshedding client (getMunicipalitiesRaw (Just p))
  case res of
    Left err    -> return $ Left err
    Right raws  -> return $ Right $ map processMunicipality raws
