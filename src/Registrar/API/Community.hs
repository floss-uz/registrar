{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.API.Community (CommunityRoutes (..), communityHandlers) where

import Registrar.Prelude

import Registrar.Database qualified as DB
import Registrar.Database.Community qualified as CM

import Registrar.Database.Types (PoolSql)
import Registrar.TelegramAuth
import Registrar.Types
import Registrar.Types (TelegramAuth (..))
import Servant.API

import Registrar.Orphans
import Servant
import Servant.Server.Generic (AsServer, AsServerT)
import UnliftIO (MonadIO (..))

type CommunityRoutes :: Type -> Type
data CommunityRoutes route = MkCommunityRoutes
  { _communities :: route :- Get '[JSON] [Community]
  , _communityByName :: route :- Capture "name" Text :> Get '[JSON] Community
  }
  deriving stock (Generic)

communityHandlers :: (PoolSql) => CommunityRoutes AsServer
communityHandlers =
  MkCommunityRoutes
    { _communities = communitiesHandler
    , _communityByName = communityByNameHandler
    }

communitiesHandler :: (PoolSql) => Handler [Community]
communitiesHandler = do
  u <- liftIO CM.getAll
  pure u

communityByNameHandler :: (PoolSql) => Text -> Handler Community
communityByNameHandler name = do
  mCommunity <- liftIO $ CM.getOne name
  case mCommunity of
    Just community -> pure community
    Nothing -> throwError err404{errBody = "Community not found"}
