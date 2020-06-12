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
import           Data.Foldable
import           Telegram.Bot.API.Utils 


--  ** Updates

data Updates = Updates
  { ok :: Bool
  , result :: [Update]
  } deriving (Show, Generic)

instance ToJSON Updates
instance FromJSON Updates


--  ** Update
-- This object represents an incoming update.
-- At most one of the optional parameters can be present in any given update.

data Update = Update
  { updateId                :: Integer -- The update's unique identifier.
  , updateMessage           :: Maybe Message -- Optional. New incoming message
  , updatEditedMessage      :: Maybe Message -- Optional. New version of a message that is known to the bot and was edited
  , updatChannelPost        :: Maybe Message -- Optional. New incoming channel post of any kind — text, photo, sticker, etc.
  , updatEditedChannelPost  :: Maybe Message --	Optional. New version of a channel post that is known to the bot and was edited
 -- , inline_query         :: Maybe InlineQuery -- Optional. New incoming inline query
 -- , chosen_inline_result :: Maybe ChosenInlineResult --	Optional. The result of an inline query that was chosen by a user and sent to their chat partner.
  , callback_query       :: Maybe CallbackQuery -- Optional. New incoming callback query
  -- , shipping_query       :: Maybe ShippingQuery -- Optional. New incoming shipping query. Only for invoices with flexible price
  -- , pre_checkout_query   :: PreCheckoutQuery -- Optional. New incoming pre-checkout query. Contains full information about checkout
  -- , poll                 :: Maybe Poll -- Optional. New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot
  -- , poll_answer          :: Maybe PollAnswer -- Optional. A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
  } deriving (Show, Generic)

instance ToJSON Update where
instance FromJSON Update where
  parseJSON (Object v) = 
    Update <$> v .: "update_id"
           <*> v .:? "message"
           <*> v .:? "edited_message"
           <*> v .:? "channel_post"
           <*> v .:? "edited_channel_post"
           <*> v .:? "callback_query"




--  ** Message
-- This object represents a message.

data Message = Message
  { messageMessageId :: Int -- ^ Unique message identifier inside this chat
  , messageFrom :: Maybe User -- ^ Sender, empty for messages sent to channels
  , messageDate :: POSIXTime -- ^ Date the message was sent in Unix time
  , messageChat :: Chat -- ^ Conversation the message belongs to
  , messageForwardFrom :: Maybe User -- ^ For forwarded messages, sender of the original message
  , messageForwardFromChat :: Maybe Chat -- ^ For messages forwarded from channels, information about the original channel
  , messageForwardFromMessageId :: Maybe Int -- ^ For messages forwarded from channels, identifier of the original message in the channel
  , messageForwardSignature :: Maybe Text -- ^ For messages forwarded from channels, signature of the post author if present
  , messageForwardDate :: Maybe POSIXTime -- ^ For forwarded messages, date the original message was sent in Unix time
  , messageReplyToMessage :: Maybe Message -- ^ For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
  , messageEditDate :: Maybe POSIXTime -- ^ Date the message was last edited in Unix time
  , messageMediaGroupId :: Maybe Text -- ^ The unique identifier of a media message group this message belongs to
  , messageAuthorSignature :: Maybe Text -- ^ Signature of the post author for messages in channels
  , messageText :: Maybe Text -- ^ For text messages, the actual UTF-8 text of the message, 0-4096 characters.
  , messageEntities :: Maybe [MessageEntity] -- ^ For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
  , messageCaptionEntities :: Maybe [MessageEntity] -- ^ For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption
  , messageAudio :: Maybe Audio -- ^ Message is an audio file, information about the file
  , messageDocument :: Maybe Document -- ^ Message is a general file, information about the file
  -- , messageGame :: Maybe Game -- ^ Message is a game, information about the game. More about games »
  , messagePhoto :: Maybe [PhotoSize] -- ^ Message is a photo, available sizes of the photo
  -- , messageSticker :: Maybe Sticker -- ^ Message is a sticker, information about the sticker
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
  , messageMigrateToChatId :: Maybe Integer -- ^ The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision Double type are safe for storing this identifier.
  , messageMigrateFromChatId :: Maybe Integer -- ^ The supergroup has been migrated from a group with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision Double type are safe for storing this identifier.
  , messagePinnedMessage :: Maybe Message -- ^ Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply.
  -- , messageInvoice :: Maybe Invoice -- ^ Message is an invoice for a payment, information about the invoice. More about payments »
  -- , messageSuccessfulPayment :: Maybe SuccessfulPayment -- ^ Message is a service message about a successful payment, information about the payment. More about payments »
  , messageReplyMarkup :: Maybe InlineKeyboardMarkup
  } deriving (Generic, Show)

instance ToJSON Message
instance FromJSON Message where
  parseJSON (Object v) = 
    Message <$> v .: "message_id"
            <*> v .:? "from"
            <*> v .: "date"
            <*> v .: "chat"
            <*> v .:? "forward_from"
            <*> v .:? "forward_from_chat"
            <*> v .:? "forward_from_message_id"
            <*> v .:? "forward_signature"
            <*> v .:? "forward_date"
            <*> v .:? "reply_to_message"
            <*> v .:? "edit_date"
            <*> v .:? "media_group_id"
            <*> v .:? "author_signature"
            <*> v .:? "text"
            <*> v .:? "entities"
            <*> v .:? "caption_entities"
            <*> v .:? "audio"
            <*> v .:? "document"
            <*> v .:? "photo"
            <*> v .:? "video"
            <*> v .:? "voice"
            <*> v .:? "video_note"
            <*> v .:? "caption"
            <*> v .:? "contact"
            <*> v .:? "location"
            <*> v .:? "venue"
            <*> v .:? "new_chat_members"
            <*> v .:? "left_chat_member"
            <*> v .:? "new_chat_title"
            <*> v .:? "new_chat_photo"
            <*> v .:? "delete_chat_photo"
            <*> v .:? "group_chat_created"
            <*> v .:? "supergroup_chat_created"
            <*> v .:? "channel_chat_created"
            <*> v .:? "migrate_to_chat_id"
            <*> v .:? "migrate_from_chat_id"
            <*> v .:? "pinned_message"
            <*> v .:? "reply_markup"

            
-- ** MessageEntity

-- | This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
  { messageEntityType :: MessageEntityType -- ^ Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), underline (underlined text), strikethrough, code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)
  , messageEntityOffset :: Int -- ^ Offset in UTF-16 code units to the start of the entity
  , messageEntityLength :: Int -- ^ Length of the entity in UTF-16 code units
  , messageEntityUrl :: Maybe Text -- ^ For “text_link” only, url that will be opened after user taps on the text
  , messageEntityUser :: Maybe User -- ^ For “text_mention” only, the mentioned user
  } deriving (Generic, Show)
  
instance ToJSON MessageEntity
instance FromJSON MessageEntity where
  parseJSON (Object v) = 
    MessageEntity <$> v .: "type"
                  <*> v .: "offset"
                  <*> v .: "length"
                  <*> v .:? "url"
                  <*> v .:? "user"


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

instance ToJSON MessageEntityType
instance FromJSON MessageEntityType where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = toTelegramName "MessageEntity" }


-- ** User

-- | This object represents a Telegram user or bot.
--
-- <https://core.telegram.org/bots/api#user>
data User = User
  { userId           :: Int     -- ^ Unique identifier for this user or bot.
  , userIsBot        :: Bool       -- ^ 'True', if this user is a bot.
  , userFirstName    :: Text       -- ^ User's or bot's first name.
  , userLastName     :: Maybe Text -- ^ User‘s or bot’s last name
  , userUsername     :: Maybe Text -- ^ User‘s or bot’s username
  , userLanguageCode :: Maybe Text -- ^ IETF language tag of the user's language
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User where
  parseJSON (Object v) = 
    User <$> v .: "id"
         <*> v .: "is_bot"
         <*> v .: "first_name"
         <*> v .:? "last_name"
         <*> v .:? "username"
         <*> v .:? "language_code"

-- ** Chat

-- | This object represents a chat.
--
-- <https://core.telegram.org/bots/api#chat>
data Chat = Chat
  { chatId                           :: ChatId          -- ^ Unique identifier for this chat. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision Double type are safe for storing this identifier.
  , chatType                         :: ChatType        -- ^ Type of chat.
  , chatTitle                        :: Maybe Text      -- ^ Title, for supergroups, channels and group chats
  , chatUsername                     :: Maybe Text      -- ^ Username, for private chats, supergroups and channels if available
  , chatFirstName                    :: Maybe Text      -- ^ First name of the other party in a private chat
  , chatLastName                     :: Maybe Text      -- ^ Last name of the other party in a private chat
  -- , chatAllMembersAreAdministrators  :: Maybe Bool      -- ^ 'True' if a group has ‘All Members Are Admins’ enabled.
  , chatPhoto                        :: Maybe ChatPhoto -- ^ Chat photo. Returned only in getChat.
  , chatDescription                  :: Maybe Text      -- ^ Description, for supergroups and channel chats. Returned only in getChat.
  , chatInviteLink                   :: Maybe Text      -- ^ Chat invite link, for supergroups and channel chats. Returned only in getChat.
  , chatPinnedMessage                :: Maybe Message   -- ^ Pinned message, for supergroups. Returned only in getChat.
  , chatStickerSetName               :: Maybe Text      -- ^ For supergroups, name of group sticker set. Returned only in getChat.
  , chatCanSetStickerSet             :: Maybe Bool      -- ^ True, if the bot can change the group sticker set. Returned only in getChat.
  } deriving (Generic, Show)

instance ToJSON   Chat
instance FromJSON Chat where
  parseJSON (Object v) = 
    Chat <$> v .: "id"
         <*> v .: "type"
         <*> v .:? "title"
         <*> v .:? "username"
         <*> v .:? "first_name"
         <*> v .:? "last_name"
         <*> v .:? "photo"
         <*> v .:? "description"
         <*> v .:? "invite_link"
         <*> v .:? "pinned_message"
         <*> v .:? "sticker_set_name"
         <*> v .:? "can_set_sticker_set"

type ChatId = Integer

-- | Type of chat.
data ChatType
  = ChatTypePrivate
  | ChatTypeGroup
  | ChatTypeSupergroup
  | ChatTypeChannel
  deriving (Generic, Show)

instance ToJSON   ChatType 
instance FromJSON ChatType where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = toTelegramName "ChatType" }
 

-- ** 'PhotoSize'

-- | This object represents one size of a photo or a file / sticker thumbnail.
data PhotoSize = PhotoSize
  { photoSizeFileId   :: Text      -- ^ Unique identifier for this file
  , photoSizeWidth    :: Int       -- ^ Photo width
  , photoSizeHeight   :: Int       -- ^ Photo height
  , photoSizeFileSize :: Maybe Int -- ^ File size
  } deriving (Generic, Show)

instance ToJSON   PhotoSize 
instance FromJSON PhotoSize where
  parseJSON (Object v) = 
    PhotoSize <$> v .: "file_id"
              <*> v .: "width"
              <*> v .: "height"
              <*> v .:? "file_size"


-- ** 'Audio'

-- | This object represents an audio file to be treated as music by the Telegram clients.
data Audio = Audio
  { audioFileId :: Text -- ^ Unique identifier for this file
  , audioDuration :: Integer -- ^ Duration of the audio in seconds as defined by sender
  , audioPerformer :: Maybe Text -- ^ Performer of the audio as defined by sender or by audio tags
  , audioTitle :: Maybe Text -- ^ Title of the audio as defined by sender or by audio tags
  , audioMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender
  , audioFileSize :: Maybe Int -- ^ File size
  } deriving (Generic, Show)

instance ToJSON   Audio 
instance FromJSON Audio where
  parseJSON (Object v) = 
    Audio <$> v .: "file_id"
          <*> v .: "duration"
          <*> v .:? "performer"
          <*> v .:? "title"
          <*> v .:? "mime_type"
          <*> v .:? "file_size"


-- ** 'Document'

-- | This object represents a general file (as opposed to photos, voice messages and audio files).
data Document = Document
  { documentFileId :: Text -- ^ Unique file identifier
  , documentThumb :: Maybe PhotoSize -- ^ Document thumbnail as defined by sender
  , documentFileName :: Maybe Text -- ^ Original filename as defined by sender
  , documentMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender
  , documentFileSize :: Maybe Int -- ^ File size
  } deriving (Generic, Show)

instance ToJSON Document 
instance FromJSON Document where
  parseJSON (Object v) = 
    Document <$> v .: "file_id"
             <*> v .:? "thumb"
             <*> v .:? "file_name"
             <*> v .:? "mime_type"
             <*> v .:? "file_size"


-- ** 'Video'

-- | This object represents a video file.
data Video = Video
  { videoFileId :: Text -- ^ Unique identifier for this file
  , videoWidth :: Int -- ^ Video width as defined by sender
  , videoHeight :: Int -- ^ Video height as defined by sender
  , videoDuration :: Int -- ^ Duration of the video in seconds as defined by sender
  , videoThumb :: Maybe PhotoSize -- ^ Video thumbnail
  , videoMimeType :: Maybe Text -- ^ Mime type of a file as defined by sender
  , videoFileSize :: Maybe Int -- ^ File size
  } deriving (Generic, Show)

instance ToJSON Video 
instance FromJSON Video where
  parseJSON (Object v) = 
    Video <$> v .: "file_id"
          <*> v .: "width"
          <*> v .: "height"
          <*> v .: "duration"
          <*> v .:? "thumb"
          <*> v .:? "mime_type"
          <*> v .:? "file_size"


-- ** 'Voice'

-- | This object represents a voice note.
data Voice = Voice
  { voiceFileId :: Text -- ^ Unique identifier for this file
  , voiceDuration :: Int -- ^ Duration of the audio in seconds as defined by sender
  , voiceMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender
  , voiceFileSize :: Maybe Int -- ^ File size
  } deriving (Generic, Show)

instance ToJSON Voice 
instance FromJSON Voice where
  parseJSON (Object v) = 
    Voice <$> v .: "file_id"
          <*> v .: "duration"
          <*> v .:? "mime_type"
          <*> v .:? "file_size"



-- ** 'VideoNote'

-- | This object represents a video message (available in Telegram apps as of v.4.0).
data VideoNote = VideoNote
  { videoNoteFileId :: Text -- ^ Unique identifier for this file
  , videoNoteLength :: Int -- ^ Video width and height as defined by sender
  , videoNoteDuration :: Int -- ^ Duration of the video in seconds as defined by sender
  , videoNoteThumb :: Maybe PhotoSize -- ^ Video thumbnail
  , videoNoteFileSize :: Maybe Int -- ^ File size
  } deriving (Generic, Show)

instance ToJSON VideoNote 
instance FromJSON VideoNote where
  parseJSON (Object v) = 
    VideoNote <$> v .: "file_id"
              <*> v .: "length"
              <*> v .: "duration"
              <*> v .:? "thumb"
              <*> v .:? "file_size"


-- ** 'Contact'

-- | This object represents a phone contact.
data Contact = Contact
  { contactPhoneNumber :: Text -- ^ Contact's phone number
  , contactFirstName :: Text -- ^ Contact's first name
  , contactLastName :: Maybe Text -- ^ Contact's last name
  , contactUserId :: Maybe Integer -- ^ Contact's user identifier in Telegram
  } deriving (Generic, Show)

instance ToJSON Contact 
instance FromJSON Contact where
  parseJSON (Object v) = 
    Contact <$> v .: "phone_number"
            <*> v .: "first_name"
            <*> v .:? "last_name"
            <*> v .:? "user_id"



-- ** Location

-- | This object represents a point on the map.
data Location = Location
  { locationLongitude :: Double -- ^ Longitude as defined by sender
  , locationLatitude  :: Double -- ^ Latitude as defined by sender
  } deriving (Generic, Show)

instance ToJSON Location 
instance FromJSON Location where
  parseJSON (Object v) = 
    Location <$> v .: "longitude"
             <*> v .: "latitude"


-- ** 'Venue'

-- | This object represents a venue.
data Venue = Venue
  { venueLocation :: Location -- ^ Venue location
  , venueTitle :: Text -- ^ Name of the venue
  , venueAddress :: Text -- ^ Address of the venue
  , venueFoursquareId :: Maybe Text -- ^ Foursquare identifier of the venue
  } deriving (Generic, Show)

instance ToJSON Venue 
instance FromJSON Venue where
  parseJSON (Object v) = 
    Venue <$> v .: "location"
          <*> v .: "title"
          <*> v .: "address"
          <*> v .:? "foursquare_id"


-- ** 'UserProfilePhotos'

-- | This object represent a user's profile pictures.
data UserProfilePhotos = UserProfilePhotos
  { userProfilePhotosTotalCount :: Int -- ^ Total number of profile pictures the target user has
  , userProfilePhotosPhotos :: [[PhotoSize]] -- ^ Requested profile pictures (in up to 4 sizes each)
  } deriving (Generic, Show)

instance ToJSON UserProfilePhotos 
instance FromJSON UserProfilePhotos  


-- ** 'File'

-- | This object represents a file ready to be downloaded.
-- The file can be downloaded via the link @https://api.telegram.org/file/bot<token>/<file_path>@.
-- It is guaranteed that the link will be valid for at least 1 hour.
-- When the link expires, a new one can be requested by calling getFile.
data File = File
  { fileFileId :: Text -- ^ Unique identifier for this file
  , fileFileSize :: Maybe Int -- ^ File size, if known
  , fileFilePath :: Maybe Text -- ^ File path. Use https://api.telegram.org/file/bot<token>/<file_path> to get the file.
  } deriving (Generic, Show)

instance ToJSON File 
instance FromJSON File    


-- ** Chat photo

-- | Chat photo. Returned only in getChat.
data ChatPhoto = ChatPhoto
  { chatPhotoSmallFileId :: Text -- ^ Unique file identifier of small (160x160) chat photo. This file_id can be used only for photo download.
  , chatPhotoBigFileId   :: Text -- ^ Unique file identifier of big (640x640) chat photo. This file_id can be used only for photo download.
  } deriving (Generic, Show)

instance ToJSON ChatPhoto 
instance FromJSON ChatPhoto  


-- ** InlineKeyboardMarkup

-- | This object represents an inline keyboard that appears right next to the message it belongs to.

data InlineKeyboardMarkup = InlineKeyboardMarkup -- Array of button rows, each represented by an Array of InlineKeyboardButton objects
  {inlineKeyboardButtons :: [[InlineKeyboardButton]]} deriving (Generic, Show) 

instance ToJSON InlineKeyboardMarkup where
  toJSON (InlineKeyboardMarkup a ) = object 
    [ "inline_keyboard" .= a ]

instance FromJSON InlineKeyboardMarkup where
  parseJSON (Object v) = 
    InlineKeyboardMarkup <$> v .: "inline_keyboard"

-- ** InlineKeyboardButton
-- | This object represents one button of an inline keyboard. You must use exactly one of the optional fields.
                      
data InlineKeyboardButton = InlineKeyboardButton 
  { inlineKeyboardButtonText :: Text -- Label text on the button
  -- , inlineKeyboardButtonUrl :: Maybe Text 
  --  , inlineKeyboardMarkupLoginUrl :: Maybe LoginUrl 
  , inlineKeyboardButtonCallbackData :: Maybe Text -- Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes
  -- , inlineKeyboardMarkupSwitchInlineQuery :: Maybe Text 
  -- , inlineKeyboardMarkupSwitchInlineQueryCurrentChat :: Maybe Text 
  --  , inlineKeyboardMarkupCallbackGame :: CallbackGame
  -- , inlineKeyboardMarkupPay :: Maybe Bool 
  } 
    deriving (Generic, Show)

instance ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton a b) = object 
    [ "text" .= a
    , "callback_data" .= b]

instance FromJSON InlineKeyboardButton where
  parseJSON (Object v) = 
    InlineKeyboardButton <$> v .: "text" 
                         <*> v .:? "callback_data"

-- ** CallbackQuery
-- | This object represents an incoming callback query from a callback button in an inline keyboard. 
-- If the button that originated the query was attached to a message sent by the bot, the field message will be present. 
-- If the button was attached to a message sent via the bot (in inline mode), the field inline_message_id will be present. 
-- Exactly one of the fields data or game_short_name will be present.

data CallbackQuery = CallbackQuery
  { callbackQueryId :: Text 
  , callbackQueryFrom :: User
  , callbackQueryMessage :: Maybe Message
  , callbackQueryInlineMessageId :: Maybe Text 
  , callbackChatInstance :: Maybe Text 
  , callbackChatData :: Maybe Text} deriving (Generic, Show)

instance ToJSON CallbackQuery 
instance FromJSON CallbackQuery where
  parseJSON (Object v) = 
    CallbackQuery <$> v .: "id" 
                  <*> v .: "from"
                  <*> v .:? "message"
                  <*> v .:? "inline_message_id"
                  <*> v .:? "chat_instance"
                  <*> v .:? "data"

