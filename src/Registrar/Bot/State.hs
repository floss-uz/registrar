module Registrar.Bot.State where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.HashMap.Strict (HashMap)
import Data.Text qualified as T
import System.Environment (getEnv)

import Data.HashMap.Strict qualified as HM
import Registrar.Bot.Types (Community)

type Model = BotState

data BotState = BotState
  { botSettings :: !Settings
  , rfc :: TVar (HashMap T.Text T.Text)
  , communities :: TVar [Community]
  }

data Settings = Settings
  { botName :: !T.Text
  , botToken :: !String
  }
  deriving (Show)

newBotState :: Settings -> IO BotState
newBotState botSettings = do
  communities <- newTVarIO []
  rfc <- newTVarIO HM.empty
  return BotState{botSettings, ..}

loadDefaultSettings :: IO Settings
loadDefaultSettings = do
  token <- getEnv "BOT_TOKEN"
  pure $ Settings{botName = "floss bot", botToken = token}

setBotState :: Settings -> BotState
setBotState botSettings = BotState{botSettings}
