{-# OPTIONS_GHC -fno-warn-orphans #-}

module Registrar.Orphans where

import Telegram.Bot.API

deriving stock instance Eq ChatType
