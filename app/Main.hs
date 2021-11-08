module Main where

import Loadshedding

main :: IO ()
main = do
  client <- newLoadsheddingClient
  status <- getLoadsheddingStatus client

  case status of
    Left err  -> print err
    Right 0   -> putStrLn "Not load shedding at the moment"
    Right l   -> putStrLn $ "Load shedding level " <> show l
