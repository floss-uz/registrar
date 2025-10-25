module Registrar.Bot.Handler where

import Registrar.Prelude

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor (void)
import GHC.Stack (HasCallStack)
import Registrar.Bot.Common
import Registrar.Bot.Reply
import Registrar.Bot.State
import Registrar.Bot.Types

import Telegram.Bot.API
import Telegram.Bot.Simple.Eff

import Data.Text qualified as T

handleAction :: (HasCallStack) => Action -> Model -> Eff Action Model
handleAction Start model =
  model <# do
    replyStart
handleAction Help model =
  model <# do
    replyHelp
handleAction Group model =
  model <# do
    c <- liftIO $ readTVarIO model.communities
    replyCommunities c
handleAction (JoinMember u chid mid) model =
  model <# do
    let msg = mentionMessageRequest u (SomeChatId chid) $ T.pack "Hi @username!"
    void $ call model $ sendMessage msg
    void $ call model $ deleteMessage chid mid
