module Registrar.Bot.Handler where

import Registrar.Prelude

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import GHC.Stack (HasCallStack)
import Registrar.Bot.Reply
import Registrar.Bot.State
import Registrar.Bot.Types
import Telegram.Bot.Simple.Eff

handleAction :: (HasCallStack) => Action -> Model -> Eff Action Model
handleAction Start model =
  model <# do
    replyStart
handleAction Group model =
  model <# do
    c <- liftIO $ readTVarIO model.communities
    replyCommunities c
