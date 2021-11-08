{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Loadshedding
    ( newLoadsheddingClient
    , LoadsheddingClient
    , LoadsheddingStatus
    , getLoadsheddingStatus
    ) where

import Data.Aeson
import Data.Proxy
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

newtype LoadsheddingClient = LoadsheddingClient { _env :: ClientEnv }

type LoadsheddingAPI =
  "getstatus" :> Get '[JSON] LoadsheddingStatus

type LoadsheddingStatus = Int

loadsheddingAPI :: Proxy LoadsheddingAPI
loadsheddingAPI = Proxy

newLoadsheddingClient :: IO LoadsheddingClient
newLoadsheddingClient = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "loadshedding.eskom.co.za" 443 "loadshedding")
  return $ LoadsheddingClient env

queryLoadshedding :: LoadsheddingClient -> ClientM a -> IO (Either ClientError a)
queryLoadshedding client query = runClientM query env
  where env = _env client

getLoadsheddingStatusRaw :: ClientM LoadsheddingStatus
getLoadsheddingStatusRaw = client loadsheddingAPI

-- The API returns 2 for stage 1 etc
-- This returns 0 for no load shedding, 1 for stage 1 etc
getLoadsheddingStatus :: LoadsheddingClient -> IO (Either ClientError LoadsheddingStatus)
getLoadsheddingStatus client = do
  res <- queryLoadshedding client getLoadsheddingStatusRaw
  case res of
    Left err  -> return $ Left err
    Right val -> return $ Right $ val - 1
