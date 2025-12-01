{-# LANGUAGE AllowAmbiguousTypes #-}

module Registrar.Bot.Types where

import Control.Lens.Internal.CTypes (Int64)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Time
import Debug.Trace
import Registrar.Bot.Common
import Registrar.Orphans
import Registrar.Prelude
import Telegram.Bot.API

newtype SpamerId = SpamerId UserId
  deriving newtype (Eq, Show, Read, Hashable)
data Action
  = Start
  | About
  | Help
  | Group
  | Useful
  | JoinMember (Maybe User) ChatId MessageId
  | Warn Update
  | ForwardCommunity SpamerId Text
  deriving (Show)

data UserInfo = UserInfo
  { uId :: UserId
  , uFirstName :: Text
  , uLastName :: Maybe Text
  , uLink :: Text
  }
  deriving (Show, Eq, Generic)

fromTelegramUser :: User -> UserInfo
fromTelegramUser u@User{..} =
  let us =
        UserInfo
          { uId = userId
          , uFirstName = userFirstName
          , uLastName = userLastName
          , uLink = makeUserLink u
          }
   in trace (show us) us

data CommunityActions = CommunityWarn SpamerId Text
  deriving stock (Eq, Show, Read)

data UserWarn = UserWarn
  { expredAt :: !UTCTime
  , val :: !Int
  , spamedChat :: !ChatId
  , spamerLink :: !Text
  , communityKeyboardListMsg :: !(Maybe MessageId)
  }
  deriving stock (Eq, Show, Generic)

type UserWarnings = HashMap SpamerId UserWarn

data WarnResult = WarnUser | BanUser deriving (Eq, Show)

newWarning :: UTCTime -> ChatId -> Text -> UserWarn
newWarning t chId link = UserWarn t 1 chId link Nothing
