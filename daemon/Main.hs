module Main where

import Config
import Loadshedding
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime

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
          now <- getCurrentTime
          next <- nextShutdown now sched
          print next
