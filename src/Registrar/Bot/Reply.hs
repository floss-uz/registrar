module Registrar.Bot.Reply where

import Data.Text qualified as T
import Registrar.Prelude
import Registrar.Types (Community (..))

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

replyHelp :: BotM ()
replyHelp = do
  reply replyMsg
 where
  replyMsg =
    (toReplyMessage $ replyAnswer ReplyHelp)

replyCommunities :: [Community] -> BotM ()
replyCommunities c = do
  reply $ replyMsg c
 where
  mkButton cm = [urlButton cm.name cm.github] -- Change github url to, chat. But chat is nullable need discussion on what to show.
  keyboard c =
    InlineKeyboardMarkup
      { inlineKeyboardMarkupInlineKeyboard =
          mkButton <$> c
      }
  replyMsg c =
    (toReplyMessage $ replyAnswer ReplyShowGroups)
      { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup (keyboard c)
      }

data ReplyAnswerType
  = ReplyStart
  | ReplyShowGroups
  | ReplyHelp

replyAnswer :: ReplyAnswerType -> Text
replyAnswer = \case
  ReplyStart ->
    T.unlines
      [ "Assalomu alaykum, hurmatli haker! "
      , "Floss jamiyati haqida hammasi shu yerda"
      ]
  ReplyShowGroups ->
    T.unlines
      [ "Floss hamjamiyatiga doir barcha guruhlar shu yerda joylashgan" -- TODO: Need correct reply text content, use multiline strings
      ]
  ReplyHelp ->
    T.unlines
      [ "📚 *Yordam bo'limi*"
      , ""
      , "*Asosiy buyruqlar:*"
      , "/start - Botni ishga tushirish"
      , "/group - Floss jamiyatining barcha guruhlari ro'yxati"
      , "/help - Ushbu yordam xabarini ko'rish"
      , ""
      , "Floss jamiyati - bu ochiq kodli dasturiy ta'minot va texnologiyalar bilan shug'ullanuvchi dasturchilar hamjamiyati."
      ]
