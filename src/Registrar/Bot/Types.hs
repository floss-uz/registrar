module Registrar.Bot.Types where

import Registrar.Prelude
import Telegram.Bot.API

data Action
  = Start
  | About
  | Help
  | Group
  | Useful
  | JoinMember (Maybe User) ChatId MessageId
  deriving (Show)
