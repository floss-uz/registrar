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

import Data.OpenApi hiding (Server)
import Registrar.API.Community
import Registrar.API.OAuth

import Control.Lens
import Data.Text qualified as T
import Registrar.API.Util
import Servant
import Servant.API.Generic
import Servant.OpenApi
import Servant.Server
import Servant.Server.Generic
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

type API :: Type -> Type
data API route = MkAPI
  { communities :: route :- "communities" :> NamedRoutes CommunityRoutes
  , auth :: route :- "auth" :> NamedRoutes OAuthRoutes
  }
  deriving stock (Generic)

apiProxy :: Proxy (ToServantApi API)
apiProxy = Proxy

data ApiServer route = MkApiServer
  { api :: route :- NamedRoutes API
  , webhook :: route :- "webhook" :> ReqBody '[JSON] Update :> Post '[JSON] ()
  , docs :: route :- SwaggerAPI
  , docsJson :: route :- "docs" :> Get '[JSON] OpenApi
  }
  deriving (Generic)

-------------------------------- Openapi config ----------------------------

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDocs :: OpenApi
swaggerDocs =
  toOpenApi apiProxy
    & info
      .~ ( mempty
             & title .~ "Registrar API"
             & license ?~ "GPL"
             & contact
               ?~ ( mempty
                      & name ?~ "API Support"
                      & url ?~ URL "http://www.floss.uz/support"
                  )
             & description ?~ "Registrar application backend endpoints"
             & version .~ "1.0"
         )

apiHandlers :: (PoolSql) => BotState -> API AsServer
apiHandlers st =
  MkAPI
    { communities = communityHandlers
    , -- , webhook = BotAPI.webhookHandler st
      auth = oAuthHandlers st.botSettings
    }

mkServer :: (PoolSql) => BotState -> ApiServer AsServer
mkServer st =
  MkApiServer
    { api = apiHandlers st
    , docs = swaggerSchemaUIServer swaggerDocs
    , webhook = undefined
    , -- webhook = liftIO $ BotAPI.webhookHandler st
      docsJson = pure swaggerDocs
    }

runApi :: (PoolSql) => BotState -> Application
runApi st =
  serveWithContext
    (Proxy @(ToServantApi ApiServer))
    errorFormatters
    (toServant $ mkServer st)
