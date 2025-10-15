{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Registrar.API (runApi) where

import Registrar.Database (Community, PoolSql)
import Registrar.Database qualified as DB

import Registrar.Bot.Webhook qualified as BotAPI

import Data.Kind (Type)
import Data.Proxy (Proxy (..))

import Servant (Application)
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic (AsServerT, genericServeT)

import Registrar.Bot.State (BotState (..), Model (..))
import Telegram.Bot.API
import UnliftIO (MonadIO (..))

type API :: Type -> Type
data API route = MkAPI
  { communities :: route :- "communities" :> NamedRoutes CommunityRoutes
  , webhook :: route :- "webhook" :> ReqBody '[JSON] Update :> Post '[JSON] ()
  }
  deriving stock (Generic)

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

apiHandler :: (PoolSql) => BotState -> API (AsServerT IO)
apiHandler st =
  MkAPI
    { communities = communityHandlers
    , webhook = BotAPI.webhookHandler st
    }

runApi :: (PoolSql) => BotState -> Application
runApi st = genericServeT liftIO $ apiHandler st
