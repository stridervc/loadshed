{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Loadshedding
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime
import Control.Concurrent (threadDelay, forkIO, ThreadId, killThread)
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

killIfThread :: Maybe ThreadId -> IO ()
killIfThread Nothing    = return ()
killIfThread (Just tid) = killThread tid

-- replace existing (if any) shutdown thread with a new one
replaceShutdown :: LoadsheddingConfig -> Maybe ThreadId -> NominalDiffTime -> IO ThreadId
replaceShutdown config thread dt = do
  killIfThread thread
  forkIO $ scheduleShutdown config dt

-- Thread that checks the loadshedding schedule
-- Pass it a ThreadId for existing shutdown thread (if any)
checkThread :: LoadsheddingConfig -> Maybe ThreadId -> IO ()
checkThread config thread = do
  -- check load shedding stage
  -- if not 0, get schedule
  client <- newLoadsheddingClient
  status <- getLoadsheddingStatus client
  case status of
    Left err              -> do
      log' "Error getting status"
      threadDelay delay
      checkThread config thread
    Right NoLoadshedding  -> do
      log' "Not load shedding"
      killIfThread thread
      threadDelay delay
      checkThread config Nothing
    Right n               -> do
      schedule <- getSchedule client n province suburb
      case schedule of
        Left err    -> do
          log' "Error getting schedule"
          threadDelay delay
          checkThread config thread
        Right sched -> do
          now <- getCurrentTime
          next <- nextShutdown now sched
          log' $ "Next : " <> T.pack (show next)
          thread' <- replaceShutdown config thread next
          threadDelay delay
          checkThread config (Just thread')

  where province  = configProvince config
        suburb    = configSuburb config
        frequency = configFrequency config
        delay     = frequency * 60 * 1000000
        log'      = logTextLn config

main = do
  config <- loadConfig
  forkProcess $ checkThread config Nothing
