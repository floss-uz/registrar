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

import Registrar.Bot.State (BotState (..), Model (..), Settings (..))
import Registrar.ClientTypes
import Telegram.Bot.API
import UnliftIO (MonadIO (..))

import Registrar.API.Community
import Registrar.API.OAuth

type API :: Type -> Type
data API route = MkAPI
  { communities :: route :- "communities" :> NamedRoutes CommunityRoutes
  , webhook :: route :- "webhook" :> ReqBody '[JSON] Update :> Post '[JSON] ()
  , auth :: route :- "auth" :> NamedRoutes OAuthRoutes
  }
  deriving stock (Generic)

apiHandler :: (PoolSql) => BotState -> API (AsServerT IO)
apiHandler st =
  MkAPI
    { communities = communityHandlers
    , webhook = BotAPI.webhookHandler st
    , auth = oAuthHandlers st.botSettings
    }

runApi :: (PoolSql) => BotState -> Application
runApi st = genericServeT liftIO $ apiHandler st
