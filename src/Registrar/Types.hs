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

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Types (Value (Object), (.=))
import Data.Data (Proxy (..), Typeable)
import Data.OpenApi
  ( HasExample (example)
  , HasSchema (schema)
  , NamedSchema (..)
  , Referenced (..)
  , Schema
  , SchemaOptions (fieldLabelModifier)
  , ToSchema (declareNamedSchema)
  , declareSchema
  , defaultSchemaOptions
  , genericDeclareNamedSchema
  )
import Data.OpenApi.Internal.ParamSchema (ToParamSchema)
import Data.Pool (Pool)
import Data.Typeable (Typeable)
import Database.Esqueleto.Experimental
import Database.Persist.SqlBackend (SqlBackend)
import Database.Persist.TH
import GHC.Records (HasField (..))
import Registrar.Orphans
import Servant.OpenApi (HasOpenApi (toOpenApi))

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
