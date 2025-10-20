module Registrar.Bot.Common where

import Data.Text qualified as T
import Registrar.Prelude (Text, Type)
import Telegram.Bot.API

type MentionMessage :: Type
data MentionMessage = MkMentionMessage
  { mOffset :: Int
  , mLength :: Int
  , mText :: Text
  }
  deriving (Show)

mkMentionMsg :: MentionMessage -> Maybe User -> MessageEntity
mkMentionMsg msg@MkMentionMessage{..} (Just user) =
  MessageEntity
    { messageEntityType = MessageEntityMention
    , messageEntityUser = Just user
    , messageEntityOffset = mOffset
    , messageEntityLength = mLength
    , messageEntityUrl = Nothing
    , messageEntityLanguage = Nothing
    , messageEntityCustomEmojiId = Nothing
    }

mentionName :: Maybe User -> Text
mentionName (Just User{userUsername}) =
  -- FIXME: configure to empty username cases
  -- Reference: https://core.telegram.org/api/mentions
  let mentionUserName = case userUsername of
        Just u -> "@" <> u
        Nothing -> ""
   in mentionUserName

buildMentionMsg :: Text -> Text -> MentionMessage
buildMentionMsg msg uname =
  let
    mText = replaceName uname msg
    mOffset = T.findIndex (== '@') mText
    mLength = T.length uname
    defMtxt =
      MkMentionMessage
        { mText
        , mOffset = 0
        , mLength
        }
   in
    case mOffset of
      Just mOffset -> defMtxt{mOffset}
      Nothing -> defMtxt{mLength = 0, mOffset = 0}
 where
  replaceName = T.replace (T.pack "@username")

mentionMessageRequest :: Maybe User -> SomeChatId -> Text -> SendMessageRequest
mentionMessageRequest u chid msg =
  let
    mName = mentionName u
    mMessage = buildMentionMsg msg mName
   in
    (defSendMessage chid mMessage.mText)
      { sendMessageMessageThreadId = Nothing -- FIXME: Need threadid of chat
      , sendMessageText = mMessage.mText
      , sendMessageChatId = chid
      , sendMessageEntities = Just [mkMentionMsg mMessage u]
      }
