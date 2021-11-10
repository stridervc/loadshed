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
    , getSuburbs
    , Suburb (..)
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

data SuburbRaw = SuburbRaw
  { rawSuburbId   :: String
  , rawSuburbText :: String
  , rawSuburbTot  :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON SuburbRaw where
  parseJSON (Object o) =
    SuburbRaw <$> o .: "id"
              <*> o .: "text"
              <*> o .: "Tot"
  parseJSON _ = undefined

data SuburbData = SuburbData
  { results :: [SuburbRaw]
  , total   :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON SuburbData where
  parseJSON (Object o) =
    SuburbData  <$> o .: "Results"
                <*> o .: "Total"
  parseJSON _ = undefined

data Suburb = Suburb
  { suburbId    :: Int
  , suburbName  :: String
  , suburbTot   :: Int
  } deriving (Eq, Show)

type LoadsheddingAPI =
        "getstatus" :> Get '[JSON] LoadsheddingStatus
  :<|>  "getmunicipalities" :> QueryParam "id" Int :> Get '[JSON] [MunicipalityRaw]
  -- this suburb spelling mistake is the APIs
  :<|>  "getsurburbdata" :> QueryParam "pagesize" Int :> QueryParam "pagenum" Int :> QueryParam "id" Int :> Get '[JSON] SuburbData

loadsheddingAPI :: Proxy LoadsheddingAPI
loadsheddingAPI = Proxy

processMunicipality :: MunicipalityRaw -> Municipality
processMunicipality raw = Municipality
  { municipalityName  = rawName raw
  , municipalityId    = read $ rawValue raw
  }

processSuburb :: SuburbRaw -> Suburb
processSuburb raw = Suburb
  { suburbId    = read $ rawSuburbId raw
  , suburbName  = rawSuburbText raw
  , suburbTot   = rawSuburbTot raw
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
getSuburbsRaw :: Maybe Int -> Maybe Int -> Maybe Int -> ClientM SuburbData
getLoadsheddingStatusRaw :<|> getMunicipalitiesRaw :<|> getSuburbsRaw = client loadsheddingAPI

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

type PerPage        = Int
type Page           = Int
type MunicipalityID = Int
type Remaining      = Int

getSuburbsPage :: LoadsheddingClient -> PerPage -> Page -> MunicipalityID -> IO (Either ClientError (Remaining, [Suburb]))
getSuburbsPage client perpage page m = do
  res <- queryLoadshedding client (getSuburbsRaw (Just perpage) (Just page) (Just m))
  case res of
    Left err    -> return $ Left err
    Right raws  -> do
      let remaining = if perpage * page > total raws then 0 else total raws - perpage * page
      return $ Right (remaining, map processSuburb $ results raws)

getSuburbsAllPages :: LoadsheddingClient -> Page -> [Suburb] -> MunicipalityID -> IO (Either ClientError [Suburb])
getSuburbsAllPages client page suburbs m = do
  res <- getSuburbsPage client 100 page m
  case res of
    Left err                    -> return $ Left err
    Right (remaining, suburbs') -> case remaining of
      0 -> return $ Right $ suburbs <> suburbs'
      _ -> getSuburbsAllPages client (page+1) (suburbs <> suburbs') m

getSuburbs :: LoadsheddingClient -> MunicipalityID -> IO (Either ClientError [Suburb])
getSuburbs client = getSuburbsAllPages client 1 []
