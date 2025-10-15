module Registrar.Bot.State where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.HashMap.Strict (HashMap)
import Data.Text qualified as T
import System.Environment (getEnv)

import Data.HashMap.Strict qualified as HM
import GHC.Generics (Generic)
import Registrar.Bot.Types (Community)
import Servant.Client (ClientEnv)
import Telegram.Bot.API (Token (..), defaultTelegramClientEnv)

type Model = BotState

data BotState = BotState
  { botSettings :: !Settings
  -- ^ Bot settings
  , clientEnv :: ClientEnv
  -- ^ Botenv
  , communities :: TVar [Community]
  }

data Settings = Settings
  { botName :: T.Text
  -- ^ Telegram bot name. Used to parse @/command\@botname@.
  , botToken :: T.Text
  -- ^ Bot token.
  , debugEnabled :: Bool
  -- ^ Whether debug enabled or not
  }
  deriving (Generic, Show)

newBotState :: Settings -> IO BotState
newBotState settings = do
  communities <- newTVarIO []
  clientEnv <- defaultTelegramClientEnv (Token . botToken $ settings)
  return BotState{botSettings = settings, ..}
