module Registrar.State (newAppState, Model (..), AppState (..), Settings (..)) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.HashMap.Strict (HashMap)
import Data.Kind (Constraint, Type)
import Data.Text qualified as T
import System.Environment (getEnv)

import Data.HashMap.Strict qualified as HM
import Registrar.Bot.Types (Community)

type Model = AppState

type AppState :: Type
data AppState = AppState
  { botSettings :: !Settings
  , rfc :: TVar (HashMap T.Text T.Text)
  , communities :: TVar [Community]
  }

type Settings :: Type
data Settings = Settings
  { botName :: !T.Text
  , botToken :: !String
  }
  deriving (Show)

newAppState :: Settings -> IO AppState
newAppState botSettings = do
  communities <- newTVarIO []
  rfc <- newTVarIO HM.empty
  return AppState{botSettings, ..}

loadDefaultSettings :: IO Settings
loadDefaultSettings = do
  token <- getEnv "BOT_TOKEN"
  pure $ Settings{botName = "floss bot", botToken = token}

setAppState :: Settings -> AppState
setAppState botSettings = AppState{botSettings}
