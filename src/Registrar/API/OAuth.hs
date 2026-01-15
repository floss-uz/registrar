{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.API.OAuth (OAuthRoutes (..), oAuthHandlers) where

import Registrar.Prelude

import Registrar.Database.Types (PoolSql)
import Registrar.TelegramAuth
import Registrar.Types (Community, TelegramAuth (..))

import Registrar.Bot.State
import Servant (Application)
import Servant.API

import Registrar.ClientTypes
import Servant
import Servant.Server.Generic (AsServer, AsServerT)
import UnliftIO (MonadIO (..), readTVar)

type OAuthRoutes :: Type -> Type
data OAuthRoutes route = MkOauthRoutes
  { _telegram :: route :- "telegram" :> ReqBody '[JSON] TelegramAuth :> Post '[JSON] AuthResp
  }
  deriving stock (Generic)

oAuthHandlers :: (PoolSql) => Settings -> OAuthRoutes AsServer
oAuthHandlers st@Settings{botToken} =
  MkOauthRoutes
    { _telegram = telegramOauth botToken
    }

telegramOauth :: (PoolSql) => Text -> TelegramAuth -> Handler AuthResp
telegramOauth tk au = do
  return $ verifyAuth tk au
