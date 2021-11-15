{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Loadshedding.Common
  ( LoadsheddingClient (..)
  , PerPage
  , Page
  , MunicipalityID
  , Remaining
  , SuburbID
  , LoadsheddingStage
  , ProvinceID
  , newLoadsheddingClient
  ) where

import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

data LoadsheddingClient = LoadsheddingClient
  { _env      :: ClientEnv
  , _manager  :: Manager
  }

type PerPage            = Int
type Page               = Int
type MunicipalityID     = Int
type Remaining          = Int
type SuburbID           = Int
type LoadsheddingStage  = Int
type ProvinceID         = Int

newLoadsheddingClient :: IO LoadsheddingClient
newLoadsheddingClient = do
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager (BaseUrl Https "loadshedding.eskom.co.za" 443 "loadshedding")
  return $ LoadsheddingClient env manager

