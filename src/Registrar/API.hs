module Registrar.API (runApi) where

import Registrar.Database
import Registrar.Database qualified as DB

import Data.Kind (Type)
import Data.Proxy (Proxy (..))

import Servant (Application)
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic (AsServerT, genericServeT)

import UnliftIO (MonadIO (..))

type API :: Type -> Type
data API route = MkAPI
  { communities :: route :- "communities" :> NamedRoutes CommunityRoutes
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

apiHandler :: (PoolSql) => API (AsServerT IO)
apiHandler =
  MkAPI
    { communities = communityHandlers
    }

runApi :: (PoolSql) => Application
runApi = genericServeT liftIO apiHandler
