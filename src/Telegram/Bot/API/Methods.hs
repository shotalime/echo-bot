module Telegram.Bot.API.Methods where

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
import           Data.Maybe                     ( fromJust, isJust )
import           Text.Casing
import           Telegram.Bot.API.Types
import           BotConfig
import qualified Data.Configurator as C

api :: Text
api = "api.telegram.org"

botToken :: Text
botToken = mconcat ["bot", token]

-- get

getLastUpdate ::  Updates -> Maybe Update
getLastUpdate updates = case result updates of
  [] -> Nothing
  _ -> Just $ last $ result updates

getFirstUpdate ::  Updates -> Maybe Update
getFirstUpdate updates = case result updates of
  [] -> Nothing
  _ -> Just $ head $ result updates

getChat :: Maybe Message -> Chat
getChat (Just m) = messageChat m

getChatId :: Update -> ChatId
getChatId = chatId . getChat . updateMessage

getText :: Update -> Maybe Text
getText u = updateMessage u >>= messageText

getOffset :: Maybe Update -> Integer
getOffset Nothing = 0
getOffset (Just update) = updateId update

getBodyResponse ::  LbsResponse -> HttpResponseBody LbsResponse
getBodyResponse  = responseBody

--messsage

data EchoMessage = 
  ForwardMessage 
    { echoMessageChatId :: ChatId
    , echoMessageFromChatId :: ChatId
    , echoMessageMessageId :: Int }  
  | BotMessage 
    { echoMessageChatId :: ChatId
    , echoMessageText :: Text }
  | InlineKeyboard 
    { echoMessageChatId :: ChatId
    , echoMessageReplyMarkup :: InlineKeyboardMarkup
    , echoMessageText :: Text 
    }
      deriving (Generic, Show)

instance ToJSON EchoMessage where
  toJSON (ForwardMessage a b c) = object 
    [ "chat_id"       .= a
    ,  "from_chat_id" .= b
    ,  "message_id"   .= c]
  toJSON (BotMessage a b) = object 
    [ "chat_id" .= a
    ,  "text"   .= b]
  toJSON (InlineKeyboard a b c) = object 
    [ "chat_id"      .= a
    , "reply_markup" .= b
    , "text"         .= c]

instance FromJSON EchoMessage 

inlineKeyboard :: InlineKeyboardMarkup 
inlineKeyboard = InlineKeyboardMarkup { 
                  inlineKeyboardButtons = 
                    [[ InlineKeyboardButton
                        { inlineKeyboardButtonText = "1"
                        , inlineKeyboardButtonCallbackData = Just "1"}
                     , InlineKeyboardButton
                        { inlineKeyboardButtonText = "2"
                        , inlineKeyboardButtonCallbackData = Just "2"} 
                     , InlineKeyboardButton
                        { inlineKeyboardButtonText = "3"
                        , inlineKeyboardButtonCallbackData = Just "3"} 
                     , InlineKeyboardButton
                        { inlineKeyboardButtonText = "4"
                        , inlineKeyboardButtonCallbackData = Just "4"} 
                     , InlineKeyboardButton
                        { inlineKeyboardButtonText = "5"
                        , inlineKeyboardButtonCallbackData = Just "5"} 
                        ]]}



sendEchoMessage :: Update -> Req LbsResponse
sendEchoMessage update =
  req POST
    (https api /: botToken /: "forwardMessage")
    (ReqBodyJson $ ForwardMessage { echoMessageChatId = getChatId update
                                  , echoMessageFromChatId = getChatId update
                                  , echoMessageMessageId = messageMessageId $ fromJust $ updateMessage update})
    lbsResponse
    mempty
--

isBotCommand :: Update -> Maybe Bool
isBotCommand u = any (\x -> messageEntityType x == MessageEntityBotCommand) <$> ( messageEntities =<< updateMessage u)

botCommand :: Update -> Req LbsResponse
botCommand update
  | command == "/help" = req POST
                          (https api /: botToken /: "sendMessage")
                          (ReqBodyJson $ BotMessage { echoMessageChatId = getChatId update
                                                    , echoMessageText = "das ist help"})
                          lbsResponse
                          mempty

  | command == "/repeat" = req POST
                          (https api /: botToken /: "sendMessage")
                          (ReqBodyJson $ InlineKeyboard { echoMessageChatId = getChatId update
                                                        , echoMessageReplyMarkup = inlineKeyboard
                                                        , echoMessageText = "select the number of repetitions"})
                          lbsResponse
                          mempty

  | otherwise           = req POST
                          (https api /: botToken /: "sendMessage")
                          (ReqBodyJson $ BotMessage { echoMessageChatId = getChatId update
                                                    , echoMessageText = "There is no such command, see /help"})
                          lbsResponse
                          mempty
    where 
      command = fromJust $ messageText =<< updateMessage update

sendMessage :: Update -> Req LbsResponse
sendMessage update = case isBotCommand update of 
  Just True  -> botCommand update 
  _          -> sendEchoMessage update



-- make update request
makeUpdateRequest :: Integer -> Req LbsResponse
makeUpdateRequest offset = req POST
                        (https api /: botToken /: "getUpdates")
                        NoReqBody
                        lbsResponse $
                        "timeout" =: (30000 :: Int) <>
                        "offset"  =: offset <>
                        responseTimeout 60000000


--run

run ::  Integer -> Req LbsResponse
run offset = do
  response <- makeUpdateRequest offset
  let body = responseBody response
  let updates = decode body :: Maybe Updates
  let lastUpdate = getLastUpdate =<< updates
  -- liftIO $ print (eitherDecode body :: Either String Updates)
  case updates of
   Nothing -> do
    liftIO $ print $ "not send " ++ show offset
    run $ getOffset lastUpdate
   _       -> do
    liftIO $ print "312"
    mapM_ sendMessage (result $ fromJust updates)
    run $ getOffset lastUpdate + 1