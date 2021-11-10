{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Loadshedding
import System.Console.CmdArgs
import qualified Data.Text as T

data Loadshed = Stage
              | Provinces
              | Municipalities { province :: Int }
              | Suburbs { municipality :: Int }
              deriving (Eq, Show, Data, Typeable)

stage = Stage &= help "Show current load shedding stage" &= auto
provinces = Provinces &= help "List province names and IDs"
municipalities = Municipalities
  { province = 3 &= help "Province ID"
  } &= help "List municipalities in province"
suburbs = Suburbs
  { municipality = 166
  } &= help "List suburbs in municipality"

knownProvinces =
  [ "Eastern Cape"
  , "Free State"
  , "Gauteng"
  , "KwaZulu-Natal"
  , "Limpopo"
  , "Mpumalanga"
  , "Norh West"
  , "Northern Cape"
  , "Western Cape"
  ]

doStage :: IO ()
doStage = do
  client <- newLoadsheddingClient
  status <- getLoadsheddingStatus client
  case status of
    Left err  -> print err
    Right 0   -> putStrLn "Not load shedding"
    Right l   -> putStrLn $ "Stage " <> show l

doProvinces :: IO ()
doProvinces = mapM_ print' $ zip [1..] knownProvinces
  where print' (i,n) = putStrLn $ show i <> " " <> n

doMunicipalities :: Int -> IO ()
doMunicipalities p = do
  client <- newLoadsheddingClient
  res <- getMunicipalities client p
  case res of
    Left err  -> print err
    Right ms  -> mapM_ print' ms
  where print' m  = putStrLn $ show (municipalityId m) <> " " <> municipalityName m

doSuburbs :: Int -> IO ()
doSuburbs m = do
  client <- newLoadsheddingClient
  res <- getSuburbs client m
  case res of
    Left err  -> print err
    Right ss  -> mapM_ print' $ filter (\s -> suburbTot s > 0) ss
  where print' s  = putStrLn $ show (suburbId s) <> " " <> suburbName s

main :: IO ()
main = do
  mode <- cmdArgsRun $ cmdArgsMode $ modes [stage, provinces, municipalities, suburbs] &= summary "loadshed 0.0.0"
  case mode of
    Stage             -> doStage
    Provinces         -> doProvinces
    Municipalities p  -> doMunicipalities p
    Suburbs m         -> doSuburbs m
