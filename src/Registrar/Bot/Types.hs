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
  | Warn Update
  | ForwardCommunity Text
  deriving (Show)

data CommunityActions = CommunityWarn Text deriving stock (Eq, Show, Read)
