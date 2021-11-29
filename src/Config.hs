{-# LANGUAGE OverloadedStrings #-}

module Config
  ( LoadsheddingConfig (..)
  , loadConfig
  ) where

import Data.Text (Text)
import qualified Data.Configurator as C

data LoadsheddingConfig = LoadsheddingConfig
  { configProvince  :: Int
  , configSuburb    :: Int
  , configFrequency :: Int
  , configEarly     :: Int
  , configShutdown  :: Text
  , configLogfile   :: Text
  } deriving (Eq, Show)

loadConfig :: IO LoadsheddingConfig
loadConfig = do
  config <- C.load [ C.Optional "/etc/loadshedding.conf", C.Optional "$(HOME)/.config/loadshedding" ]

  cp <- C.lookupDefault 3 config "province"
  cs <- C.lookupDefault 1021456 config "suburb"
  cf <- C.lookupDefault 10 config "frequency"
  ce <- C.lookupDefault 2 config "early"
  cc <- C.lookupDefault "echo 'Unconfigured'" config "shutdown"
  cl <- C.lookupDefault "/tmp/loadshedding.log" config "logfile"

  return $ LoadsheddingConfig
    { configProvince  = cp
    , configSuburb    = cs
    , configFrequency = cf
    , configEarly     = ce
    , configShutdown  = cc
    , configLogfile   = cl
    }
