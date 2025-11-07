{-# OPTIONS_GHC -fno-warn-orphans #-}

module Registrar.Orphans where

import Data.OpenApi (ToSchema)
import Data.OpenApi.ParamSchema
import Database.Esqueleto (PersistCore (BackendKey), SqlBackend)
import Telegram.Bot.API

deriving stock instance Eq ChatType

deriving anyclass instance ToSchema (BackendKey SqlBackend)
deriving anyclass instance ToParamSchema (BackendKey SqlBackend)
