module Registrar.Bot.Common where

import Data.Text qualified as T
import Registrar.Prelude (Text)
import Telegram.Bot.API

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

mentionName (Just User{userUsername}) =
  let mentionUserName = case userUsername of
        Just u -> "@" <> u
        Nothing -> ""
   in mentionUserName

data MentionMessage = MkMentionMessage
  { mOffset :: Int
  , mLength :: Int
  , mText :: Text
  }
  deriving (Show)

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
