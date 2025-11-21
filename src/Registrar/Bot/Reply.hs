module Registrar.Bot.Reply where

import Data.Text qualified as T
import Registrar.Prelude
import Registrar.Types (Community (..))

import Registrar.Bot.Types
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
    toReplyMessage $ replyAnswer ReplyHelp

replyAbout :: BotM ()
replyAbout = do
  reply replyMsg
 where
  replyMsg =
    toReplyMessage $ replyAnswer ReplyAbout

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
  | ReplyAbout

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
      [ "📚 *Buyruqlar ro'yxati:*"
      , ""
      , "👤 *Asosiy:*"
      , "/start - Botni boshlash"
      , "/help - Yordam"
      , "/about - Bot haqida ma'lumot"
      , "/rules - Qoidalar (std.floss.uz)"
      , ""
      , "👥 *Jamiyat:*"
      , "/community - Hamjamiyatlar ro'yxati"
      , "/members <?id> - A'zolarni qidirish"
      , "/auth - Telegram ni floss.uz bilan bog'lash"
      , ""
      , "🛠 *Hissa qo'shish:*"
      , "/publish <link> - GitHub PR e'lon qilish"
      , "/contribute <?id> - E'lon qilingan PRlar"
      , "/guide <?repo> - Hissa qo'shish bo'yicha qo'llanma"
      , ""
      , "⚠️ *Moderatsiya (adminlar uchun):*"
      , "/warn <reply> - Xabarni tegishli guruhga yo'naltirish"
      , "/report <reply> - Shikoyat yuborish"
      ]
  ReplyAbout ->
    T.unlines
      [ "👋 Assalomu alaykum! Men Registrar botman."
      , ""
      , "Men FLOSS-UZ hamjamiyatining markaziy platformasi bo'lib, loyihalar, a'zolar va resurslarni boshqaraman."
      , ""
      , "✨ Asosiy vazifalarim:"
      , "• Hamjamiyatni avtomatik moderatsiya qilish"
      , "• Resurslarni ulashishda yordam berish"
      , "• Hamjamiyatlar o'rtasida aloqa o'rnatish"
      , "• Yangi a'zolarni qabul qilish"
      , ""
      , "💡 Batafsil ma'lumot: /help"
      ]

replyCommunitiesCB :: [Community] -> BotM ()
replyCommunitiesCB c = do
  reply $ replyMsg c
 where
  mkButton cm = [actionButton cm.name (CommunityWarn cm.name)]
  keyboard c =
    InlineKeyboardMarkup
      { inlineKeyboardMarkupInlineKeyboard =
          mkButton <$> c
      }
  replyMsg c =
    (toReplyMessage $ replyAnswer ReplyShowGroups)
      { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup (keyboard c)
      }
