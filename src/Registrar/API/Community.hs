{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.API.Community (CommunityRoutes (..), communityHandlers) where

import Registrar.Prelude

import Registrar.Database qualified as DB

import Registrar.TelegramAuth
import Registrar.Types (Community, PoolSql, TelegramAuth (..))

import Servant.API

import Servant.Server.Generic (AsServerT)
import UnliftIO (MonadIO (..))

type CommunityRoutes :: Type -> Type
data CommunityRoutes route = MkCommunityRoutes
  { _communities :: route :- Get '[JSON] [Community]
  }
  deriving stock (Generic)

communityHandlers :: (PoolSql) => CommunityRoutes (AsServerT IO)
communityHandlers =
  MkCommunityRoutes
    { _communities = DB.communityList
    }
