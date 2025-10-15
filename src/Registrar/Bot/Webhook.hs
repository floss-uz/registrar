module Registrar.Bot.Webhook
  ( webhookHandler
  ) where

import Registrar.Prelude

import Control.Concurrent.STM (readTVarIO)
import Data.Functor (void)

import Registrar.Bot qualified as BT
import Registrar.Bot.Handler (handleAction)
import Registrar.Bot.State (BotState (..), Model (..))
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.BotApp.Internal

webhookHandler :: BotState -> Update -> IO ()
webhookHandler st update = do
  env <- BT.startBotEnv (BT.bot st) st.clientEnv
  x <- handle (BT.bot st) env
  return ()
 where
  -- TODO: refactor handle function
  handle BotApp{..} botEnv@BotEnv{..} = liftIO $ liftIO $ void . asyncLink $ do
    maction <- botAction update <$> readTVarIO botModelVar
    case maction of
      Nothing -> return ()
      Just action -> issueAction botEnv (Just update) (Just action)
