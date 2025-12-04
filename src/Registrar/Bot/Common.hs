module Registrar.Bot.Common where

import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Registrar.Orphans
import Registrar.Prelude (Text, Type)
import Telegram.Bot.API
import Text.Show.Unicode

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

utrm :: Text -> Text
utrm = (T.pack . ushow . T.unpack)

makeName :: (a -> Text) -> (a -> Maybe Text) -> a -> Text
makeName firstNameSelector lastNameSelector v =
  T.concat [firstNameSelector v, maybe "" ("" <>) (lastNameSelector v)]

getUserName :: User -> Text
getUserName = makeName userFirstName userLastName

getChatName :: Chat -> Text
getChatName = makeName (fromMaybe "" . chatFirstName) chatLastName

getChatFullInfoName :: ChatFullInfo -> Text
getChatFullInfoName = makeName (fromMaybe "" . chatFullInfoFirstName) chatFullInfoLastName

makeChatLink :: ChatFullInfo -> Text
makeChatLink =
  makeLink
    "chat"
    (coerce @_ @Integer . chatFullInfoId)
    chatFullInfoUsername
    getChatFullInfoName

makeUserLink :: User -> Text
makeUserLink = makeLink "user" (coerce @_ @Integer . userId) userUsername getUserName

makeLink :: Text -> (a -> Integer) -> (a -> Maybe Text) -> (a -> Text) -> a -> Text
makeLink entityType idSelector usernameSelector getName v = case usernameSelector v of
  Nothing ->
    T.concat
      [ "<a href=\"tg://"
      , entityType
      , "?id="
      , T.pack $ show (idSelector v)
      , "\">"
      , getName v
      , "</a>"
      ]
  Just username -> T.cons '@' username

restrictedUserPermissions :: ChatPermissions
restrictedUserPermissions =
  ChatPermissions
    { chatPermissionsCanSendMessages = Nothing
    , chatPermissionsCanSendAudios = Nothing
    , chatPermissionsCanSendDocuments = Nothing
    , chatPermissionsCanSendPhotos = Nothing
    , chatPermissionsCanSendVideos = Nothing
    , chatPermissionsCanSendVideoNotes = Nothing
    , chatPermissionsCanSendVoiceNotes = Nothing
    , chatPermissionsCanSendPolls = Nothing
    , chatPermissionsCanSendOtherMessages = Nothing
    , chatPermissionsCanAddWebPagePreviews = Nothing
    , chatPermissionsCanChangeInfo = Nothing
    , chatPermissionsCanInviteUsers = Nothing
    , chatPermissionsCanPinMessages = Nothing
    , chatPermissionsCanManageTopics = Nothing
    }
