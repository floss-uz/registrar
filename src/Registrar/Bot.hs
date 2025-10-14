module Registrar.Bot
  ( regulatorBot
  ) where

import Data.Text qualified as T

import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict qualified as HM
import Network.Wai.Handler.Warp
import Registrar.Bot.Handler
import Registrar.Bot.Parse
import Registrar.Bot.State
import Registrar.Bot.Types
import Telegram.Bot.API
import Telegram.Bot.Simple

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

-- runBot :: String -> IO ()
-- runBot botToken = do
--   st <- newBotState Settings{botName = "floss bot", botToken}

--   print "Floss regulator bot ready for run"
--   where
