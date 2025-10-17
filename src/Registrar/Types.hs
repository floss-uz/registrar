{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.Types
  ( Community (..)
  , CommunityId (..)
  , PoolSql (..)
  ) where

import Registrar.Prelude

import Data.Pool (Pool)
import Database.Esqueleto.Experimental
import Database.Persist.SqlBackend (SqlBackend)
import Database.Persist.TH

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
