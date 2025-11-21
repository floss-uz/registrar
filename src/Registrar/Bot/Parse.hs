module Registrar.Bot.Parse where

import Registrar.Prelude

import Telegram.Bot.API
import Telegram.Bot.Simple.UpdateParser

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (asum)
import Data.Maybe
import Registrar.Bot.Message
import Registrar.Bot.State
import Registrar.Bot.Types
import Text.Read (readMaybe)

import Data.Text qualified as T

updateToAction :: Settings -> Update -> Maybe Action
updateToAction settings@Settings{..} update
  | isCommand "start" update = handleStart
  | isCommand "help" update = handleHelp
  | isCommand "about" update = handleAbout
  | isCommand "community" update = handleGroup
  | isCommand "warn" update = handleWarn settings update
  | isCallback update = handleCallback settings =<< updateCallbackQuery update
  | otherwise = handleMessage settings update
 where
  isCallback = isJust . updateCallbackQuery
  isCommand cmd = isJust . parseUpdate (commandWithBotName botName cmd)

handleStart :: Maybe Action
handleStart = Just Start

handleHelp :: Maybe Action
handleHelp = Just Help

handleAbout :: Maybe Action
handleAbout = Just About

handleGroup :: Maybe Action
handleGroup = Just Group

handleMessage :: Settings -> Update -> Maybe Action
handleMessage st@Settings{..} u@Update{..}
  | Just msg <- asum [updateMessage, updateEditedMessage] =
      case parseMsgSource msg of
        PublicGroup chatId _ -> handleGroupMessage chatId msg
        _ -> Nothing
  | otherwise = Nothing

handleGroupMessage :: ChatId -> Message -> Maybe Action
handleGroupMessage chatId msg@Message{..} =
  let
    isJoinMsg = isJust messageNewChatMembers
   in
    if isJoinMsg then Just $ JoinMember messageFrom chatId messageMessageId else Nothing

handleWarn :: Settings -> Update -> Maybe Action
handleWarn st updt =
  Just $ Warn updt

handleCallback :: Settings -> CallbackQuery -> Maybe Action
handleCallback Settings{..} q@CallbackQuery{..} =
  let warnCommunity = readMaybe @CommunityActions =<< fmap T.unpack callbackQueryData
   in case warnCommunity of
        Just (CommunityWarn txt) -> Just $ ForwardCommunity txt
        Nothing -> Nothing
