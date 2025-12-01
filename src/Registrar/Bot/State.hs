module Registrar.Bot.State where

import Registrar.Prelude

import Control.Concurrent.STM (TVar, newTVar, newTVarIO, readTVarIO)
import Data.HashMap.Strict (HashMap)
import System.Environment (getEnv)

import Control.Concurrent
import Control.Concurrent.STM (modifyTVar', readTVar)
import Control.Concurrent.STM.TVar (writeTVar)
import Control.Exception.Safe (SomeException, try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (atomically)
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Registrar.Bot.Types (SpamerId, UserWarn (..), UserWarnings, WarnResult (..), newWarning)
import Registrar.Types (Community)
import Servant.Client
import Telegram.Bot.API (ChatId, Token (..), UserId, defaultTelegramClientEnv)
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
  , userWarns :: TVar UserWarnings
  }

data WarnSetting = WarnSetting
  { userWarningInterval :: Int
  , userWarnLimit :: Int
  }
  deriving (Show)

data Settings = Settings
  { botName :: Text
  -- ^ Telegram bot name. Used to parse @/command\@botname@.
  , botToken :: Text
  -- ^ Bot token.
  , debugEnabled :: Bool
  -- ^ Whether debug enabled or not
  , warnSetting :: WarnSetting
  }
  deriving (Generic, Show)

defaultWarnSetting = WarnSetting 7200 3

newBotState :: Settings -> [Community] -> IO BotState
newBotState settings cm = do
  communities <- newTVarIO cm
  requestLock <- newMVar ()
  clientEnv <- defaultTelegramClientEnv (Token . botToken $ settings)
  userWarns <- newTVarIO $ HM.empty
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

warnUser
  :: (MonadIO m)
  => WarnSetting
  -> UTCTime
  -> TVar UserWarnings
  -> SpamerId
  -> Text
  -> ChatId
  -> m WarnResult
warnUser ws now userWarnings uid spamerLink chId = do
  now <- liftIO getCurrentTime
  uws <- liftIO $ readTVarIO userWarnings
  let (wcnt, rs) = warn ws now $ HM.lookup uid uws
      updateWarn = case rs of
        BanUser -> HM.delete uid
        WarnUser -> HM.insertWith (\new _ -> new) uid wcnt
  liftIO $ atomically $ modifyTVar' userWarnings $ updateWarn
  return rs
 where
  newWarn = newWarning now chId spamerLink
  warn :: WarnSetting -> UTCTime -> Maybe UserWarn -> (UserWarn, WarnResult)
  warn _ now Nothing = (newWarn, WarnUser)
  warn ws now (Just wc)
    | timeExpired now wc.expredAt ws.userWarningInterval =
        (newWarn, WarnUser)
    | otherwise =
        let new = wc{val = succ wc.val} -- fixme
         in if new.val >= ws.userWarnLimit
              then (new, BanUser)
              else (new, WarnUser)
