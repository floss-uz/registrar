{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.Types
  ( Community (..)
  , TelegramAuth (..)
  ) where

import Registrar.Prelude
import Registrar.Types.Community

import Data.Data (Proxy (..))
import Data.OpenApi (ToSchema)
import Data.Pool (Pool)
import Database.Esqueleto.Experimental
import Database.Persist.SqlBackend (SqlBackend)
import Database.Persist.TH
import GHC.Records (HasField (..))

type TelegramAuth :: Type
data TelegramAuth = MkTelegramAuth
  { id :: !Int
  , auth_date :: !Int
  , first_name :: !(Maybe String)
  , last_name :: !(Maybe String)
  , photo_url :: !(Maybe String)
  , username :: !(Maybe String)
  , token :: !Text
  , hash :: !String
  }

deriving stock instance Show TelegramAuth
deriving stock instance Generic TelegramAuth
deriving anyclass instance ToJSON TelegramAuth
deriving anyclass instance FromJSON TelegramAuth
deriving anyclass instance ToSchema TelegramAuth
