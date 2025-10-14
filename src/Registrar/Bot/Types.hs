module Registrar.Bot.Types where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics

data Action
  = Start
  | About
  | Help
  | Group
  | Useful
  deriving (Show, Read)

data Community = Community
  { name :: !T.Text
  , established :: !Int
  , mission :: !T.Text
  , description :: !T.Text
  , chat :: !(Maybe T.Text)
  , manager :: !(Maybe T.Text)
  , github :: !T.Text
  , website :: !(Maybe T.Text)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)
