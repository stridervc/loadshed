{-# LANGUAGE OverloadedStrings #-}

module Loadshedding.Schedule
  ( ScheduleDay (..)
  , ScheduleInfo (..)
  , getSchedule
  ) where

import Loadshedding.Common

import Network.HTTP.Client (parseRequest, httpLbs, Response, responseBody)
import Data.ByteString.Lazy (ByteString)

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.ByteString.Lazy (Parser)

type InfoLabel    = String
type DayMonth     = String
type TimeStr      = String
type TimeRange    = (TimeStr, TimeStr)
type DaySchedule  = (DayMonth, [TimeRange])

data ScheduleDay = ScheduleDay
  { scheduleDate  :: String
  , scheduleTimes :: [String]
  } deriving (Eq, Show)

data ScheduleInfo = ScheduleInfo
  { infoLabels    :: [String]
  , scheduleDays  :: [ScheduleDay]
  } deriving (Eq, Show)

getSchedule :: LoadsheddingClient -> LoadsheddingStage -> ProvinceID -> SuburbID -> IO (Either P.ParseError [DaySchedule])
getSchedule client stage pid sid = do
  req <- parseRequest $ "https://loadshedding.eskom.co.za/loadshedding/getschedulem/" <> sid' <> "/" <> stage' <> "/" <> pid' <> "/1"
  response <- httpLbs req manager
  let parsed = P.parse scheduleParser "(scheduleParser)" $ responseBody response
  --return $ responseBody response
  return parsed
  where sid'    = show sid
        pid'    = show pid
        stage'  = show stage
        manager = _manager client

-- parse misc info
infoLabelParser :: Parser InfoLabel
infoLabelParser = do
  P.string "\"areaInfoLabel\">"
  info <- P.many1 P.letter
  P.string "\r\n"
  return info

-- parse day month in the form of "Tue, 16 Nov"
dayMonthParser :: Parser DayMonth
dayMonthParser = do
  P.manyTill P.anyChar $ P.try $ P.string "dayMonth\">"
  P.spaces
  P.manyTill P.anyChar $ P.try $ P.char '\r'

-- parse basic time in the form of "04:00"
timeParser :: Parser TimeStr
timeParser = do
  hour <- P.many1 P.digit
  P.char ':'
  minute <- P.many1 P.digit
  return $ hour ++ ":" ++ minute

-- parse time range in the form of "04:00 - 06:30"
timeRangeParser :: Parser TimeRange
timeRangeParser = do
  P.manyTill P.anyChar $ P.try $ P.string "Time\":\""
  P.manyTill P.anyChar $ P.try $ P.string ";>"
  time1 <- timeParser
  P.string " - "
  time2 <- timeParser
  P.manyTill P.anyChar $ P.try $ P.string "</div>"
  P.spaces
  return (time1, time2)

dayScheduleParser :: Parser DaySchedule
dayScheduleParser = do
  daymonth <- dayMonthParser
  timeranges <- P.manyTill timeRangeParser $ P.try $ P.string "</div>"
  return (daymonth, timeranges)

scheduleParser :: Parser [DaySchedule]
scheduleParser = do
  P.many $ P.try dayScheduleParser

