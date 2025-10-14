module Registrar.Bot.Webhook (webhookHandler, webhookAPI, WebhookAPI (..)) where

import Registrar.Bot

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void)
import Servant
  ( Application
  , Handler
  , JSON
  , Post
  , Proxy (..)
  , ReqBody
  , Server
  , serve
  , type (:>)
  )

import Telegram.Bot.API.GettingUpdates (Update)
import Telegram.Bot.Simple.BotApp.Internal

type WebhookAPI = ReqBody '[JSON] Update :> Post '[JSON] ()

webhookHandler :: BotApp model action -> BotEnv model action -> Server WebhookAPI
webhookHandler BotApp{..} botEnv@BotEnv{..} =
  updateHandler
 where
  updateHandler :: Update -> Handler ()
  updateHandler update = liftIO $ handleUpdate update
  handleUpdate update = liftIO . void . asyncLink $ do
    maction <- botAction update <$> readTVarIO botModelVar
    case maction of
      Nothing -> return ()
      Just action -> issueAction botEnv (Just update) (Just action)

webhookAPI :: Proxy WebhookAPI
webhookAPI = Proxy

-- app :: BotApp model action -> BotEnv model action -> Application
-- app botEnv = serve webhookAPI $ webhookHandler botApp botEnv

-- app env = serve webhookAPI $ webhookHandler (regulatorBot env) env
