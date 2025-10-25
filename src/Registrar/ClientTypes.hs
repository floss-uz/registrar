module Registrar.ClientTypes where

import Registrar.Prelude (FromJSON, Generic, Text, ToJSON, Type)
import Servant.Server

type AuthResp :: Type
data AuthResp = MkAuthResp
  { success :: !Bool
  }
  deriving stock (Generic, Show)

deriving anyclass instance ToJSON AuthResp
