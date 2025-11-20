{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.Database.Types where

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
  Member sql=members
    username String            -- unique, optionally oidc synced
    full_name Text Maybe       -- optionally oidc synced
    u_email Text               -- oidc synced
    u_status Text              -- oidc synced
    u_blog Text                -- let it be website or telegram
    u_github Text              -- username
    u_telegram Text            -- username
    u_mastodon Text            -- @username@instance
    matrix Text
    chair CommunityId Maybe
    deriving Eq
  Ban sql=bans
    user MemberId
    reason Text
    expire Text -- FIXME: must time
    -- communities -- FIXME: need implement
    -- status -- FIXME: need implement
    deriving Eq
|]

type Community :: Type
type CommunityId :: Type
type Member :: Type
type MemberId :: Type
type Ban :: Type
type BanId :: Type

deriving stock instance Generic Community
deriving anyclass instance FromJSON Community
deriving stock instance Generic Member
deriving anyclass instance FromJSON Member
deriving stock instance Generic Ban
deriving anyclass instance FromJSON Ban
