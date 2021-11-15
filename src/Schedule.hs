{-# LANGUAGE OverloadedStrings #-}

module Schedule
  ( ScheduleDay (..)
  , ScheduleInfo (..)
  ) where

data ScheduleDay = ScheduleDay
  { scheduleDate  :: String
  , scheduleTimes :: [String]
  } deriving (Eq, Show)

data ScheduleInfo = ScheduleInfo
  { infoLabels    :: [String]
  , scheduleDays  :: [ScheduleDay]
  } deriving (Eq, Show)

type SuburbID   = Int
type Stage      = Int
type ProvinceID = Int

getSchedule :: Stage -> ProvinceID -> SuburbID -> IO ()
getSchedule stage pid sid = undefined

