module Registrar.Bot.Types where

import Registrar.Prelude

data Action
  = Start
  | About
  | Help
  | Group
  | Useful
  deriving (Show, Read)
