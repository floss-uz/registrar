{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.API.OAuth (OAuthRoutes (..), oAuthHandlers) where

import Registrar.Prelude

import Registrar.TelegramAuth
import Registrar.Types (Community, PoolSql, TelegramAuth (..))

import Registrar.Bot.State
import Servant (Application)
import Servant.API

import Registrar.ClientTypes
import Servant.Server.Generic (AsServerT)
import UnliftIO (MonadIO (..))

type OAuthRoutes :: Type -> Type
data OAuthRoutes route = MkOauthRoutes
  { _telegram :: route :- "telegram" :> ReqBody '[JSON] TelegramAuth :> Post '[JSON] AuthResp
  }
  deriving stock (Generic)

oAuthHandlers :: (PoolSql) => Settings -> OAuthRoutes (AsServerT IO)
oAuthHandlers st@Settings{botToken} =
  MkOauthRoutes
    { _telegram = verifyAuth botToken
    }
