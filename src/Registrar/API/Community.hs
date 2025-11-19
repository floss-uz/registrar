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

import Servant.Server.Generic (AsServer, AsServerT)
import UnliftIO (MonadIO (..))

type CommunityRoutes :: Type -> Type
data CommunityRoutes route = MkCommunityRoutes
  { _communities :: route :- Get '[JSON] [Community]
  }
  deriving stock (Generic)

communityHandlers :: (PoolSql) => CommunityRoutes AsServer
communityHandlers =
  MkCommunityRoutes
    { _communities = liftIO CM.getAll -- FIXME: implement handler
    }
