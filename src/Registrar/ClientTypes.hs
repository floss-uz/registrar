module Registrar.ClientTypes where

import Data.OpenApi (ToSchema)
import Registrar.Prelude (FromJSON, Generic, Text, ToJSON, Type)
import Servant.Server

type AuthResp :: Type
data AuthResp = MkAuthResp
  { success :: !Bool
  }
  deriving stock (Generic, Show)

deriving anyclass instance ToJSON AuthResp
deriving anyclass instance ToSchema AuthResp
