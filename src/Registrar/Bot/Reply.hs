module Registrar.Bot.Reply where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Registrar.Bot.State (BotState (..))
import Registrar.Bot.Types (Community (..))
import Telegram.Bot.API
import Telegram.Bot.Simple

replyStart :: BotM ()
replyStart = do
  reply replyMsg
 where
  mkButton = uncurry urlButton
  keyboard =
    InlineKeyboardMarkup
      { inlineKeyboardMarkupInlineKeyboard =
          [mkButton <$> [("Jamiyat", "https://t.me/flossuzc"), ("Web Sahifa", "https://floss.uz")]]
      }
  replyMsg =
    (toReplyMessage $ replyAnswer ReplyStart)
      { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
      }

replyCommunities :: BotState -> BotM ()
replyCommunities model = do
  c <- liftIO $ readTVarIO model.communities
  reply $ replyMsg c
 where
  mkButton cm = urlButton cm.name cm.github -- Change github url to, chat. But chat is nullable need discussion on what to show.
  keyboard c =
    InlineKeyboardMarkup
      { inlineKeyboardMarkupInlineKeyboard =
          [mkButton <$> c]
      }
  replyMsg c =
    (toReplyMessage $ replyAnswer ReplyShowGroups)
      { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup (keyboard c)
      }

data ReplyAnswerType
  = ReplyStart
  | ReplyShowGroups

replyAnswer :: ReplyAnswerType -> Text.Text
replyAnswer = \case
  ReplyStart ->
    Text.unlines
      [ "Assalomu alaykum, hurmatli haker! "
      , "Floss jamiyati haqida hammasi shu yerda"
      ]
  ReplyShowGroups ->
    Text.unlines
      [ "Floss hamjamiyatiga doir barcha guruhlar shu yerda joylashgan" -- TODO: Need correct reply text content, use multiline strings
      ]
