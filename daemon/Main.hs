{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Loadshedding
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime
import Control.Concurrent (threadDelay, forkIO)
import System.Posix.Process (forkProcess)
import System.Process (spawnCommand)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

nextShutdownT :: DaySchedule -> IO ()
nextShutdownT (day, times) = do
  -- Thu, 18 Nov
  -- 10:00
  now <- getCurrentTime
  let (year, month, dom) = toGregorian $ utctDay now
  let time' = show year <> " " <> day <> " " <> fst (head times) <> " +0200"
  parsed <- (parseTimeM False defaultTimeLocale "%Y %a, %d %b %R %z" time' :: IO UTCTime)

  print now
  print parsed
  print $ diffUTCTime parsed now

-- change a DaySchedule to just be a list of strings of day and start time
-- we add the year and timezone as well
-- for example : "2021 Thu, 18 Nov 10:00 +0200"
simplifyDaySchedule :: Integer -> DaySchedule -> [String]
simplifyDaySchedule _ (_,[]) = []
simplifyDaySchedule year (day, tr:trs) = simplified : simplifyDaySchedule year (day, trs)
  where simplified  = show year <> " " <> day <> " " <> fst tr <> " +0200"

nextShutdown :: UTCTime -> [DaySchedule] -> IO NominalDiffTime
nextShutdown now schedules = do
  let (year, _, _) = toGregorian $ utctDay now
  let times = concatMap (simplifyDaySchedule year) schedules
  times' <- mapM parse' times
  return $ head $ filter (> 0) $ map (`diffUTCTime` now) times'
  where parse'  :: String -> IO UTCTime
        parse'  = parseTimeM False defaultTimeLocale "%Y %a, %d %b %R %z"

logTextLn :: LoadsheddingConfig -> T.Text -> IO ()
logTextLn config text = do
  now <- getCurrentTime
  let timestr = T.pack $ formatTime defaultTimeLocale "%c" now

  TIO.appendFile filename $ timestr <> " loadsheddingd: " <> text <> "\n"
  where filename = T.unpack $ configLogfile config

-- thread that waits and performs the shutdown
scheduleShutdown :: LoadsheddingConfig -> NominalDiffTime -> IO ()
scheduleShutdown config dt = do
  threadDelay $ floor $ if delay < 0 then 0 else delay
  logTextLn config "Shutting down system"
  spawnCommand $ T.unpack $ configShutdown config
  return ()
  where delay = (toRational dt - toRational (configEarly config) * 60) * 1000000

-- Thread that checks the loadshedding schedule
checkThread :: LoadsheddingConfig -> IO ()
checkThread config = do
  -- check load shedding stage
  -- if not 0, get schedule
  -- show next loadshedding start time
  client <- newLoadsheddingClient
  status <- getLoadsheddingStatus client
  case status of
    Left err              -> log' "Error getting status"
    Right NoLoadshedding  -> log' "Not load shedding"
    Right n               -> do
      schedule <- getSchedule client n province suburb
      case schedule of
        Left err    -> log' "Error getting schedule"
        Right sched -> do
          now <- getCurrentTime
          next <- nextShutdown now sched
          log' $ "Next : " <> T.pack (show next)

  threadDelay delay
  checkThread config
  where province  = configProvince config
        suburb    = configSuburb config
        frequency = configFrequency config
        delay     = frequency * 60 * 1000000
        log'      = logTextLn config

main = do
  config <- loadConfig
  forkProcess $ checkThread config
