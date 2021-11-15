{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Loadshedding.Municipality
  ( MunicipalityRaw (..)
  , Municipality (..)
  , processMunicipality
  ) where

import Loadshedding.Common

import Data.Aeson
import GHC.Generics
import Servant.Client

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

processMunicipality :: MunicipalityRaw -> Municipality
processMunicipality raw = Municipality
  { municipalityName  = rawName raw
  , municipalityId    = read $ rawValue raw
  }
