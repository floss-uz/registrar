module Registrar.Bot.Types where

import Registrar.Prelude

data Action
  = Start
  | About
  | Help
  | Group
  | Useful
  deriving (Show, Read)

data Community = Community
  { name :: !Text
  , established :: !Int
  , mission :: !Text
  , description :: !Text
  , chat :: !(Maybe Text)
  , manager :: !(Maybe Text)
  , github :: !Text
  , website :: !(Maybe Text)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)
