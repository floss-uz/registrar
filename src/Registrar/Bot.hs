module Registrar.Bot
  ( regulatorBot
  , bot
  , startBotEnv
  ) where

import Registrar.Prelude

import Data.Text qualified as T

import Data.HashMap.Strict qualified as HM
import Registrar.Bot.Handler
import Registrar.Bot.Parse
import Registrar.Bot.State
import Registrar.Bot.Types
import Servant.Client
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.BotApp.Internal

regulatorBot :: Model -> BotApp Model Action
regulatorBot st =
  BotApp
    { botInitialModel = st
    , botAction = actionParser
    , botHandler = handleAction
    , botJobs = []
    }
 where
  actionParser update BotState{..} = updateToAction botSettings update

bot :: Model -> BotApp (HM.HashMap (Maybe ChatId) Model) (Maybe ChatId, Action)
bot st = conversationBot updateChatId (regulatorBot st)

startBotEnv :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
startBotEnv bot env = do
  botEnv <- defaultBotEnv bot env
  _jobThreadIds <- scheduleBotJobs botEnv (botJobs bot)
  _actionsThreadId <- processActionsIndefinitely bot botEnv
  return botEnv
