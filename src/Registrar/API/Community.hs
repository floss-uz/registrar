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
import Registrar.Types (Community, PoolSql, TelegramAuth (..), WrapId)

import Servant.API

import Registrar.Orphans
import Servant
import Servant.Server.Generic (AsServer, AsServerT)
import UnliftIO (MonadIO (..))

type CommunityRoutes :: Type -> Type
data CommunityRoutes route = MkCommunityRoutes
  { _communities :: route :- Get '[JSON] [WrapId Community]
  }
  deriving stock (Generic)

communityHandlers :: (PoolSql) => CommunityRoutes AsServer
communityHandlers =
  MkCommunityRoutes
    { _communities = communitiesHandler
    }

communitiesHandler :: (PoolSql) => Handler [WrapId Community]
communitiesHandler = do
  u <- liftIO DB.communitiesWithId
  pure u
