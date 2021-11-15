{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Loadshedding.Suburb
  ( SuburbRaw (..)
  , SuburbData (..)
  , Suburb (..)
  , processSuburb
  ) where

import Loadshedding.Common

import Data.Aeson
import GHC.Generics
import Servant.Client

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

processSuburb :: SuburbRaw -> Suburb
processSuburb raw = Suburb
  { suburbId    = read $ rawSuburbId raw
  , suburbName  = rawSuburbText raw
  , suburbTot   = rawSuburbTot raw
  }
