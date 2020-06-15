module Telegram.Bot.API.Methods where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text                     as T
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


api :: T.Text
api = "api.telegram.org"

botToken :: T.Text
botToken = mconcat ["bot", token]

type Token = T.Text
type Offset = Integer

-- get

getUpdates :: LbsResponse -> Maybe [Update]
getUpdates response = do 
  let body = responseBody response
  result <$> decode body 

getLastUpdate :: Maybe [Update] -> Maybe Update
getLastUpdate updates = case updates of 
  Nothing -> Nothing 
  Just [] -> Nothing 
  Just xs -> Just $ last xs
                       
getFirstUpdate ::  Maybe [Update] -> Maybe Update
getFirstUpdate updates = case updates of 
  Nothing -> Nothing 
  Just [] -> Nothing 
  Just xs -> Just $ head xs

getNumOfRep :: LbsResponse ->  Int
getNumOfRep response = read $ fromJust $ callbackQueryData $ fromJust $ callbackQuery $ fromJust $ getLastUpdate $ getUpdates response

-- getChat :: Maybe Message -> Maybe CallbackQuery -> Chat
-- getChat Nothing c = calmessageChat

getChatId :: Update -> ChatId
getChatId u = case updateMessage u of
  Nothing -> chatId $ messageChat $ fromJust $ callbackQueryMessage =<< callbackQuery u
  _       -> chatId $ messageChat $ fromJust $ updateMessage u

getMessageId :: Update -> Int
getMessageId u = case updateMessage u of
  Nothing -> messageMessageId $ fromJust $ callbackQueryMessage =<< callbackQuery u
  _       -> messageMessageId $ fromJust $ updateMessage u

getText :: Update -> Maybe T.Text
getText u = updateMessage u >>= messageText

getOffset :: Maybe Update -> Offset
getOffset Nothing = 0
getOffset (Just update) = updateId update


--messsage

data EchoMessage =
  ForwardMessage
    { echoMessageChatId :: ChatId
    , echoMessageFromChatId :: ChatId
    , echoMessageMessageId :: Int }
  | BotMessage
    { echoMessageChatId :: ChatId
    , echoMessageText :: T.Text }
  | InlineKeyboard
    { echoMessageChatId :: ChatId
    , echoMessageReplyMarkup :: InlineKeyboardMarkup
    , echoMessageText :: T.Text
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

-- send message


sendEchoMessage :: Token -> Update -> Req LbsResponse
sendEchoMessage token update =
  req POST
    (https api /: token /: "forwardMessage")
    (ReqBodyJson $ ForwardMessage { echoMessageChatId = getChatId update
                                  , echoMessageFromChatId = getChatId update
                                  , echoMessageMessageId = getMessageId update})
    lbsResponse
    mempty

--

isBotCommand :: Update -> Maybe Bool
isBotCommand u =  any (\x -> messageEntityType x == MessageEntityBotCommand) <$> ( messageEntities =<< updateMessage u)

botCommand :: Token -> Update -> Req LbsResponse
botCommand token update
  | command == "/help" = req POST
                          (https api /: token /: "sendMessage")
                          (ReqBodyJson $ BotMessage { echoMessageChatId = getChatId update
                                                    , echoMessageText = "das ist help"})
                          lbsResponse
                          mempty

  | command == "/repeat" = req POST
                          (https api /: token /: "sendMessage")
                          (ReqBodyJson $ InlineKeyboard { echoMessageChatId = getChatId update
                                                        , echoMessageReplyMarkup = inlineKeyboard
                                                        , echoMessageText = "select the number of repetitions"})
                          lbsResponse
                          mempty

  | otherwise           = req POST
                          (https api /: token /: "sendMessage")
                          (ReqBodyJson $ BotMessage { echoMessageChatId = getChatId update
                                                    , echoMessageText = "There is no such command, see /help"})
                          lbsResponse
                          mempty
    where
      command = fromJust $ messageText =<< updateMessage update


-- make update request
makeUpdateRequest :: Offset -> Token -> Req LbsResponse
makeUpdateRequest offset token = req POST
                        (https api /: token /: "getUpdates")
                        NoReqBody
                        lbsResponse $
                        "timeout" =: (30000 :: Int) <>
                        "offset"  =: offset <>
                        responseTimeout 60000000

--run

run :: Token -> Offset -> Int -> Req LbsResponse
run token offset numOfRep = do
  response <- makeUpdateRequest offset token

  let updates = getUpdates response
  let lastUpdate = getLastUpdate updates

  case updates of
    Nothing ->
      run token (getOffset lastUpdate) numOfRep
    _       -> case isBotCommand =<< lastUpdate of
              Just True -> do
                botCommand token (fromJust lastUpdate)
                response' <- makeUpdateRequest (getOffset lastUpdate + 1) token
                let lastUpdate' = getLastUpdate $ getUpdates response'
                liftIO $ print (getNumOfRep response')
                run token (getOffset lastUpdate' + 1) (getNumOfRep response')
              _         -> do
                -- liftIO $ print $ lastUpdate
                replicateM_ numOfRep $ mapM_ (sendEchoMessage token) (fromJust updates)
                run token (getOffset lastUpdate + 1) numOfRep