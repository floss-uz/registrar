{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Registrar.Bot.Message (MessageFrom (..), parseMsgSource) where

import Registrar.Orphans

import Data.Maybe (fromMaybe, isJust)
import Registrar.Prelude (Type)
import Telegram.Bot.API

type MessageFrom :: Type
data MessageFrom
  = OwnerGroup
  | DirectMessage UserId
  | PublicGroup ChatId UserId
  | PrivateGroup ChatId UserId
  | Unsupported ChatId

parseMsgSource :: Message -> MessageFrom
parseMsgSource Message{..} =
  let
    isGroup = messageChat.chatType `elem` [ChatTypeGroup, ChatTypeSupergroup]
    title = chatTitle messageChat
    cid = fromMaybe (chatId messageChat) messageMigrateToChatId
    isJoinMsg = isJust messageNewChatMembers

    withUser t = case userId <$> messageFrom of
      Nothing -> Unsupported cid
      Just uid -> t uid
   in
    case (isGroup, isJust title) of
      (True, True) -> withUser (PublicGroup cid)
      (True, False) -> withUser (PrivateGroup cid)
      (False, _) -> withUser DirectMessage
