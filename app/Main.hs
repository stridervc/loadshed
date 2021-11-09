module Main where

import Loadshedding

provinces = [ "Eastern Cape"
            , "Free State"
            , "Gauteng"
            , "KwaZulu-Natal"
            , "Limpopo"
            , "Mpumalanga"
            , "Norh West"
            , "Northern Cape"
            , "Western Cape"
            ]

showProvinces :: IO ()
showProvinces = mapM_ print' $ zip [1..] provinces
  where print' (i,n) = putStrLn $ show i <> " " <> n

main :: IO ()
main = do
  client <- newLoadsheddingClient
  status <- getLoadsheddingStatus client

  showProvinces
  putStrLn ""

  case status of
    Left err  -> print err
    Right 0   -> putStrLn "Not load shedding"
    Right l   -> putStrLn $ "Stage " <> show l
