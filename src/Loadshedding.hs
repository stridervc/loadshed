{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Loadshedding
    ( newLoadsheddingClient
    , LoadsheddingClient
    , LoadsheddingStage (..)
    , MunicipalityID
    , ProvinceID
    , SuburbID
    , getLoadsheddingStatus
    , getMunicipalities
    , Municipality (..)
    , getSuburbs
    , Suburb (..)
    , getSchedule
    , DaySchedule
    , stageFromInt
    ) where

import Loadshedding.Common
import Loadshedding.Suburb
import Loadshedding.Schedule
import Loadshedding.Municipality

import GHC.Generics
import Data.Aeson
import Data.Proxy
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

type LoadsheddingAPI =
        "getstatus" :> Get '[JSON] Int
  :<|>  "getmunicipalities" :> QueryParam "id" Int :> Get '[JSON] [MunicipalityRaw]
  -- this suburb spelling mistake is the API's
  :<|>  "getsurburbdata" :> QueryParam "pagesize" Int :> QueryParam "pagenum" Int :> QueryParam "id" Int :> Get '[JSON] SuburbData

loadsheddingAPI :: Proxy LoadsheddingAPI
loadsheddingAPI = Proxy

queryLoadshedding :: LoadsheddingClient -> ClientM a -> IO (Either ClientError a)
queryLoadshedding client query = runClientM query env
  where env = _env client

getLoadsheddingStatusRaw  :: ClientM Int
getMunicipalitiesRaw      :: Maybe Int -> ClientM [MunicipalityRaw]
getSuburbsRaw :: Maybe Int -> Maybe Int -> Maybe Int -> ClientM SuburbData
getLoadsheddingStatusRaw :<|> getMunicipalitiesRaw :<|> getSuburbsRaw = client loadsheddingAPI

-- The API returns 2 for stage 1 etc
-- This returns 0 for no load shedding, 1 for stage 1 etc
getLoadsheddingStatus :: LoadsheddingClient -> IO (Either ClientError LoadsheddingStage)
getLoadsheddingStatus client = do
  res <- queryLoadshedding client getLoadsheddingStatusRaw
  case res of
    Left err  -> return $ Left err
    Right val -> return $ Right $ stageFromInt val

getMunicipalities :: LoadsheddingClient -> Int -> IO (Either ClientError [Municipality])
getMunicipalities client p = do
  res <- queryLoadshedding client (getMunicipalitiesRaw (Just p))
  case res of
    Left err    -> return $ Left err
    Right raws  -> return $ Right $ map processMunicipality raws

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
