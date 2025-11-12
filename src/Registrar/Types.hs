{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.Types
  ( Community (..)
  , CommunityId (..)
  , PoolSql (..)
  , TelegramAuth (..)
  , WrapId (..)
  ) where

import Registrar.Prelude

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

type PoolSql :: Constraint
type PoolSql = (?pool :: Pool SqlBackend)

share
  [mkPersist sqlSettings{mpsPrefixFields = False}]
  [persistLowerCase|
  Community sql=communities
    name Text
    established Text
    mission Text
    chat Text Maybe
    manager Text Maybe
    github Text
    website Text Maybe
    deriving Eq 
|]

type CommunityId :: Type

deriving stock instance Generic CommunityId
deriving stock instance Typeable CommunityId
deriving anyclass instance ToSchema CommunityId
deriving anyclass instance ToParamSchema CommunityId

type Community :: Type
deriving stock instance Show Community
deriving stock instance Generic Community
deriving anyclass instance ToJSON Community
deriving anyclass instance FromJSON Community
deriving anyclass instance ToSchema Community

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

-- | This record for transforming entity to flat object.
data WrapId record = WrapId
  { recd :: record
  , wKey :: Key record
  }
  deriving stock (Generic)

deriving anyclass instance Typeable WrapId
instance (ToJSON (Key record), ToJSON record) => ToJSON (WrapId record) where
  toJSON WrapId{..} =
    Object $
      "id" .= wKey
        <> case toJSON recd of
          Object o -> o
          _ -> mempty

instance (ToSchema (Key record), ToSchema record) => ToSchema (WrapId record) where
  declareNamedSchema _ = do
    -- FIXME: implement key schema as {id: 1}. And change namedSchema to schema.
    recSchema <- declareSchema (Proxy @record)
    keySchema <- declareSchema (Proxy @(Key record))
    let
      fullSchm = recSchema <> keySchema
    pure $ NamedSchema (Just "WrapId") $ fullSchm
