{-# LANGUAGE OverloadedStrings #-}

module Loadshedding.Schedule
  ( ScheduleDay (..)
  , ScheduleInfo (..)
  , getSchedule
  ) where

import Loadshedding.Common

import Network.HTTP.Client (parseRequest, httpLbs, Response, responseBody)
-- import Network.HTTP.Client.TLS ()
import Data.ByteString.Lazy (ByteString)

data ScheduleDay = ScheduleDay
  { scheduleDate  :: String
  , scheduleTimes :: [String]
  } deriving (Eq, Show)

data ScheduleInfo = ScheduleInfo
  { infoLabels    :: [String]
  , scheduleDays  :: [ScheduleDay]
  } deriving (Eq, Show)

getSchedule :: LoadsheddingClient -> LoadsheddingStage -> ProvinceID -> SuburbID -> IO ByteString
getSchedule client stage pid sid = do
  req <- parseRequest $ "https://loadshedding.eskom.co.za/loadshedding/getschedulem/" <> sid' <> "/" <> stage' <> "/" <> pid' <> "/1"
  response <- httpLbs req manager
  return $ responseBody response
  where sid'    = show sid
        pid'    = show pid
        stage'  = show stage
        manager = _manager client

