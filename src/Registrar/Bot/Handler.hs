{-# LANGUAGE OverloadedLabels #-}

module Registrar.Bot.Handler where

import Registrar.Prelude

import Control.Concurrent.STM (modifyTVar', readTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor (void)
import GHC.Stack (HasCallStack)
import Registrar.Bot.Common
import Registrar.Bot.Reply
import Registrar.Bot.State
import Registrar.Bot.Types

import Registrar.Prelude
import Telegram.Bot.API
import Telegram.Bot.Simple.Eff

import Control.Lens ((&), (.~), (?~), (^?))
import Control.Monad (liftM, when)
import Control.Monad.STM (atomically)
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, getCurrentTime, getTimeZone)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)

handleAction :: (HasCallStack) => Action -> Model -> Eff Action Model
handleAction Start model =
  model <# do
    replyStart
handleAction Help model =
  model <# do
    replyHelp
handleAction About model =
  model <# do
    replyAbout
handleAction Group model =
  model <# do
    c <- liftIO $ readTVarIO model.communities
    replyCommunities c
handleAction (JoinMember u chid mid) model =
  model <# do
    let msg = mentionMessageRequest u (SomeChatId chid) $ T.pack "Hi @username!"
    void $ call model $ sendMessage msg
    void $ call model $ deleteMessage chid mid
handleAction (Warn u) model =
  model <# do
    let restrictionInterval = 120 -- FIXME: move to config
        rs = fromJust u.updateMessage
        mf = fromJust $ messageFrom $ fromJust rs.messageReplyToMessage -- change to safe way
        spamerId = SpamerId mf.userId
        chatId = rs.messageChat.chatId
        warnSt = model.botSettings.warnSetting
        spamer = fromTelegramUser mf

    c <- liftIO $ readTVarIO model.communities
    now <- liftIO getCurrentTime
    wr <- liftIO $ warnUser warnSt now model.userWarns spamerId spamer.uLink chatId

    let tsc = utcTimeToPOSIXSeconds now
        utc5 = addUTCTime (60 * 60 * 5) now
        fTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" utc5

    let restrictReq = (defRestrictChatMember (SomeChatId rs.messageChat.chatId) mf.userId) restrictedUserPermissions
    curTimeS <- liftIO $ currentTimeSec
    void $ call model $ restrictChatMember $ restrictReq{restrictChatMemberUntilDate = Just $ curTimeS + restrictionInterval}
    case wr of
      (BanUser) -> do
        let restrictReq = (defRestrictChatMember (SomeChatId rs.messageChat.chatId) mf.userId) restrictedUserPermissions
        curTimeS <- liftIO $ currentTimeSec
        void $ call model $ restrictChatMember $ restrictReq{restrictChatMemberUntilDate = Just $ curTimeS + restrictionInterval}
        liftIO $ print "ban"
        let req = restrictionAttention rs.messageChat.chatId mf $ T.pack fTime
        void $ call model $ sendMessage req
      -- send mention message about ban information
      (WarnUser) -> do
        liftIO $ print "warn only"
        let req = replyCommunitiesCB c spamerId $ SomeChatId chatId
        mResponse <- call model $ sendMessage req
        wcu <- liftIO $ readTVarIO model.userWarns
        forM_ mResponse $ \Response{..} -> when responseOk $ do
          let msgId = messageMessageId responseResult
              wrnC = (wcu ! spamerId) & #communityKeyboardListMsg ?~ msgId
          liftIO $ atomically $ modifyTVar' model.userWarns $ HM.insertWith (\new _ -> new) spamerId wrnC
          return ()
handleAction (ForwardCommunity sId txt) model =
  model <# do
    liftIO $ print $ txt
    uw <- liftIO $ readTVarIO model.userWarns
    when (HM.member sId uw) (warnSpamer uw)
 where
  warnSpamer uw = do
    let spamer = uw ! sId
        req = moveSpamerToChat spamer.spamedChat txt spamer.spamerLink
    void $ call model $ sendMessage req
    case spamer.communityKeyboardListMsg of
      Just msgId -> do
        void $ call model $ deleteMessage spamer.spamedChat msgId
      Nothing -> do
        liftIO $ print "Message id not found"
