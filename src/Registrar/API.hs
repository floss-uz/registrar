{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.API (runApi) where

import Registrar.Prelude

import Registrar.Database qualified as DB
import Registrar.TelegramAuth
import Registrar.Types (Community, PoolSql, TelegramAuth (..))

import Data.ByteString.Lazy qualified as BL
import Registrar.Bot.Webhook qualified as BotAPI

import Data.Proxy (Proxy (..))

import Servant (Application)
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic (AsServerT, genericServeT)

import Registrar.Bot.State (BotState (..), Model (..))
import Telegram.Bot.API
import UnliftIO (MonadIO (..))

import Registrar.Bot.State (Settings (..))

type API :: Type -> Type
data API route = MkAPI
  { communities :: route :- "communities" :> NamedRoutes CommunityRoutes
  , webhook :: route :- "webhook" :> ReqBody '[JSON] Update :> Post '[JSON] ()
  , auth :: route :- "auth" :> NamedRoutes AuthRoutes
  }
  deriving stock (Generic)

type CommunityRoutes :: Type -> Type
data CommunityRoutes route = MkCommunityRoutes
  { _communities :: route :- Get '[JSON] [Community]
  }
  deriving stock (Generic)

data AuthRoutes route = MkAuthRoutes
  { _telegram :: route :- "telegram" :> ReqBody '[JSON] TelegramAuth :> Post '[JSON] AuthResp
  }
  deriving stock (Generic)

communityHandlers :: (PoolSql) => CommunityRoutes (AsServerT IO)
communityHandlers =
  MkCommunityRoutes
    { _communities = DB.communityList
    }

authHandlers :: (PoolSql) => BotState -> AuthRoutes (AsServerT IO)
authHandlers st@BotState{botSettings} =
  MkAuthRoutes
    { _telegram = verifyAuth botSettings.botToken
    }

apiHandler :: (PoolSql) => BotState -> API (AsServerT IO)
apiHandler st =
  MkAPI
    { communities = communityHandlers
    , webhook = BotAPI.webhookHandler st
    , auth = authHandlers st
    }

runApi :: (PoolSql) => BotState -> Application
runApi st = genericServeT liftIO $ apiHandler st
