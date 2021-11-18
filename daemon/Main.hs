module Main where

import Config
import Loadshedding
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime

nextShutdown :: DaySchedule -> IO ()
nextShutdown (day, times) = do
  -- Thu, 18 Nov
  -- 10:00
  now <- getCurrentTime
  let (year, month, dom) = toGregorian $ utctDay now
  let time' = show year <> " " <> day <> " " <> fst (head times) <> " +0200"
  parsed <- (parseTimeM False defaultTimeLocale "%Y %a, %d %b %R %z" time' :: IO UTCTime)

  print now
  print parsed
  print $ diffUTCTime parsed now

main = do
  config <- loadConfig
  let province  = configProvince config
  let suburb    = configSuburb config

  -- check load shedding stage
  -- if not 0, get schedule
  -- show next loadshedding start time
  client <- newLoadsheddingClient
  status <- getLoadsheddingStatus client
  case status of
    Left err  -> putStrLn "Error getting status"
    Right 0   -> putStrLn "Not load shedding"
    Right n   -> do
      schedule <- getSchedule client n province suburb
      case schedule of
        Left err    -> putStrLn "Error getting schedule"
        Right sched -> do
          nextShutdown $ head sched
