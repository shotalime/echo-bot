module Telegram.Bot.API.Types where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics
import           Network.HTTP.Req
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as L
import qualified Text.URI                      as URI
import           Data.Time.Clock.POSIX          ( POSIXTime )
data Updates = Updates
  { ok :: Bool
  , results :: [Update]} deriving (Show, Generic)

instance FromJSON Updates

-- Update
-- This object represents an incoming update.
-- At most one of the optional parameters can be present in any given update.

data Update = Update
  { update_id            :: Integer -- The update's unique identifier.
  , message              :: Maybe Message -- Optional. New incoming message
  , edited_message       :: Maybe Message -- Optional. New version of a message that is known to the bot and was edited
  , channel_post         :: Maybe Message -- Optional. New incoming channel post of any kind — text, photo, sticker, etc.
  , edited_channel_post  :: Maybe Message --	Optional. New version of a channel post that is known to the bot and was edited
  , inline_query         :: Maybe InlineQuery -- Optional. New incoming inline query
  , chosen_inline_result :: Maybe ChosenInlineResult --	Optional. The result of an inline query that was chosen by a user and sent to their chat partner.
  , callback_query       :: Maybe CallbackQuery -- Optional. New incoming callback query
  , shipping_query       :: Maybe ShippingQuery -- Optional. New incoming shipping query. Only for invoices with flexible price
  , pre_checkout_query   :: PreCheckoutQuery -- Optional. New incoming pre-checkout query. Contains full information about checkout
  , poll                 :: Maybe Poll -- Optional. New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot
  , poll_answer          :: Maybe PollAnswer -- Optional. A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
  }

-- Message
-- This object represents a message.

data Message = Message
  { messageMessageId :: Int -- ^ Unique message identifier inside this chat
  , messageFrom :: Maybe User -- ^ Sender, empty for messages sent to channels
  , messageDate :: POSIXTime -- ^ Date the message was sent in Unix time
  , messageChat :: Chat -- ^ Conversation the message belongs to
  , messageForwardFrom :: Maybe User -- ^ For forwarded messages, sender of the original message
  , messageForwardFromChat :: Maybe Chat -- ^ For messages forwarded from channels, information about the original channel
  , messageForwardFromMessageId :: Maybe MessageId -- ^ For messages forwarded from channels, identifier of the original message in the channel
  , messageForwardSignature :: Maybe Text -- ^ For messages forwarded from channels, signature of the post author if present
  , messageForwardDate :: Maybe POSIXTime -- ^ For forwarded messages, date the original message was sent in Unix time
  , messageReplyToMessage :: Maybe Message -- ^ For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
  , messageEditDate :: Maybe POSIXTime -- ^ Date the message was last edited in Unix time
  , messageMediaGroupId :: Maybe MediaGroupId -- ^ The unique identifier of a media message group this message belongs to
  , messageAuthorSignature :: Maybe Text -- ^ Signature of the post author for messages in channels
  , messageText :: Maybe Text -- ^ For text messages, the actual UTF-8 text of the message, 0-4096 characters.
  , messageEntities :: Maybe [MessageEntity] -- ^ For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
  , messageCaptionEntities :: Maybe [MessageEntity] -- ^ For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption
  , messageAudio :: Maybe Audio -- ^ Message is an audio file, information about the file
  , messageDocument :: Maybe Document -- ^ Message is a general file, information about the file

--  , messageGame :: Maybe Game -- ^ Message is a game, information about the game. More about games »

  , messagePhoto :: Maybe [PhotoSize] -- ^ Message is a photo, available sizes of the photo

--  , messageSticker :: Maybe Sticker -- ^ Message is a sticker, information about the sticker

  , messageVideo :: Maybe Video -- ^ Message is a video, information about the video
  , messageVoice :: Maybe Voice -- ^ Message is a voice message, information about the file
  , messageVideoNote :: Maybe VideoNote -- ^ Message is a video note, information about the video message
  , messageCaption :: Maybe Text -- ^ Caption for the audio, document, photo, video or voice, 0-200 characters
  , messageContact :: Maybe Contact -- ^ Message is a shared contact, information about the contact
  , messageLocation :: Maybe Location -- ^ Message is a shared location, information about the location
  , messageVenue :: Maybe Venue -- ^ Message is a venue, information about the venue
  , messageNewChatMembers :: Maybe [User] -- ^ New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)
  , messageLeftChatMember :: Maybe User -- ^ A member was removed from the group, information about them (this member may be the bot itself)
  , messageNewChatTitle :: Maybe Text -- ^ A chat title was changed to this value
  , messageNewChatPhoto :: Maybe [PhotoSize] -- ^ A chat photo was change to this value
  , messageDeleteChatPhoto :: Maybe Bool -- ^ Service message: the chat photo was deleted
  , messageGroupChatCreated :: Maybe Bool -- ^ Service message: the group has been created
  , messageSupergroupChatCreated :: Maybe Bool -- ^ Service message: the supergroup has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a supergroup when it is created. It can only be found in reply_to_message if someone replies to a very first message in a directly created supergroup.
  , messageChannelChatCreated :: Maybe Bool -- ^ Service message: the channel has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a channel when it is created. It can only be found in reply_to_message if someone replies to a very first message in a channel.
  , messageMigrateToChatId :: Maybe ChatId -- ^ The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , messageMigrateFromChatId :: Maybe ChatId -- ^ The supergroup has been migrated from a group with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , messagePinnedMessage :: Maybe Message -- ^ Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply.

--  , messageInvoice :: Maybe Invoice -- ^ Message is an invoice for a payment, information about the invoice. More about payments »
--  , messageSuccessfulPayment :: Maybe SuccessfulPayment -- ^ Message is a service message about a successful payment, information about the payment. More about payments »
  } deriving (Generic, Show)

-- ** MessageEntity

-- | This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
  { messageEntityType :: MessageEntityType -- ^ Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), underline (underlined text), strikethrough, code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)
  , messageEntityOffset :: Int -- ^ Offset in UTF-16 code units to the start of the entity
  , messageEntityLength :: Int -- ^ Length of the entity in UTF-16 code units
  , messageEntityUrl :: Maybe Text -- ^ For “text_link” only, url that will be opened after user taps on the text
  , messageEntityUser :: Maybe User -- ^ For “text_mention” only, the mentioned user
  } deriving (Generic, Show)

-- | Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), underline (underlined text), strikethrough, code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames), cashtag, phone_number
data MessageEntityType
  = MessageEntityMention
  | MessageEntityHashtag
  | MessageEntityBotCommand
  | MessageEntityUrl
  | MessageEntityEmail
  | MessageEntityBold
  | MessageEntityItalic
  | MessageEntityUnderline -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_underline.html>
  | MessageEntityStrikethrough -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_strikethrough.html>
  | MessageEntityCode
  | MessageEntityPre
  | MessageEntityTextLink
  | MessageEntityTextMention
  | MessageEntityCashtag -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_cashtag.html>.
  | MessageEntityPhoneNumber -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_phone_number.html>.
  deriving (Eq, Show, Generic)
