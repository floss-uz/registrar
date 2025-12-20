{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Registrar.Config where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Toml qualified
import Toml.Schema (FromValue, ToTable, ToValue)
import Toml.Schema.Generic (GenericTomlTable (..))
import Toml.Schema.Matcher (Result)

data AppConfig = AppConfig
  { port :: Int
  , database :: Text
  , databasePoolSize :: Int
  , migrations :: Bool
  , datasetFolder :: Text
  , botSettings :: BotSettings
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable AppConfig

data BotSettings = BotSettings
  { botToken :: Text
  , botName :: Text
  , debugEnabled :: Bool
  , warnSetting :: WarnSetting
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable BotSettings

data WarnSetting = WarnSetting
  { userWarningInterval :: Int
  , userWarnLimit :: Int
  , permanentBanDuration :: Int
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable WarnSetting

loadConfig :: Result String AppConfig
loadConfig = Toml.decode' $ decodeUtf8 . "config.toml"
