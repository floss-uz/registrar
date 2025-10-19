module Registrar.Bot.State where

import Registrar.Prelude

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.HashMap.Strict (HashMap)
import System.Environment (getEnv)

import Control.Concurrent
import Control.Exception.Safe (SomeException, try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.HashMap.Strict qualified as HM
import Registrar.Types (Community)
import Servant.Client
import Telegram.Bot.API (Token (..), defaultTelegramClientEnv)
import Telegram.Bot.Simple

type Model = BotState

data BotState = BotState
  { botSettings :: !Settings
  -- ^ Bot settings
  , clientEnv :: ClientEnv
  -- ^ Botenv
  , requestLock :: MVar ()
  -- ^ For locking before using clientEnv
  , communities :: TVar [Community]
  }

data Settings = Settings
  { botName :: Text
  -- ^ Telegram bot name. Used to parse @/command\@botname@.
  , botToken :: Text
  -- ^ Bot token.
  , debugEnabled :: Bool
  -- ^ Whether debug enabled or not
  }
  deriving (Generic, Show)

newBotState :: Settings -> [Community] -> IO BotState
newBotState settings cm = do
  communities <- newTVarIO cm
  requestLock <- newMVar ()
  clientEnv <- defaultTelegramClientEnv (Token . botToken $ settings)
  return BotState{botSettings = settings, ..}

withLock :: (Show a, MonadIO m) => BotState -> ClientM a -> m (Maybe a)
withLock BotState{clientEnv, requestLock} action = liftIO $ do
  takeMVar requestLock
  eResult <- runClientM action clientEnv
  case eResult of
    Left err -> print err
    _ -> pure ()
  let mResult = either (const Nothing) Just eResult
  putMVar requestLock ()
  pure mResult

runAction :: ClientM (Maybe a) -> BotM (Maybe a)
runAction action = do
  liftClientM $
    try action >>= \case
      Left (e' :: SomeException) -> liftIO $ print e' >> pure Nothing
      Right result -> pure $! result

call :: (Show a) => BotState -> ClientM a -> BotM (Maybe a)
call model action = runAction $ withLock model action
