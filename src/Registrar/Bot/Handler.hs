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
handleAction Group model =
  model <# do
    c <- liftIO $ readTVarIO model.communities
    replyCommunities c
handleAction (JoinMember u chid mid) model =
  model <# do
    liftIO $ print (chid, mid)
    let msg = greetToNewcomers u $ SomeChatId chid
    rs <- call model $ sendMessage msg
    liftIO $ print rs
    void $ call model $ deleteMessage chid mid

greetToNewcomers :: Maybe User -> SomeChatId -> SendMessageRequest
greetToNewcomers = mkRmsg
 where
  mkRmsg u chid =
    let
      mName = mentionName u
      mMessage = buildMentionMsg msgTxt mName
     in
      (defSendMessage chid mMessage.mText)
        { sendMessageMessageThreadId = Nothing -- FIXME: Need threadid of chat
        , sendMessageText = mMessage.mText
        , sendMessageChatId = chid
        , sendMessageEntities = Just [mkMentionMsg mMessage u]
        }
  msgTxt = T.pack "Hi @username!"
