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
  ) where

import Registrar.Prelude

import Data.Data (Proxy (..))
import Data.OpenApi (ToSchema)
import Data.Pool (Pool)
import Database.Esqueleto.Experimental
import Database.Persist.SqlBackend (SqlBackend)
import Database.Persist.TH
import GHC.Records (HasField (..))

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

type Community :: Type
type CommunityId :: Type

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
