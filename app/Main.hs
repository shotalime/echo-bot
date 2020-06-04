module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Data.Aeson
import qualified Data.Text                     as T
import           GHC.Generics
import           Network.HTTP.Req
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as L
import           Data.Maybe                     ( fromJust )
import qualified Text.URI                      as URI
import           Telegram.Bot.API.Types
import           BotConfig
-- import           System.Timeout


data MessageToSend =  MessageToSend
  { chat_id  :: Int
  , text :: T.Text
  } deriving (Show, Generic)



instance ToJSON MessageToSend
instance FromJSON MessageToSend

api :: T.Text
api = "api.telegram.org"

botToken :: T.Text
botToken = mconcat ["bot", token]

getOffset :: Maybe Update -> Integer
getOffset Nothing = 0
getOffset (Just update) = updateId update

getLastUpdate ::  Updates -> Maybe Update
getLastUpdate updates = case result updates of
  [] -> Nothing
  _ -> Just $ last $ result updates

getMessage :: Update -> Maybe Message
getMessage = updateMessage

getChat :: Maybe Message -> Chat
getChat (Just m) = messageChat m

getChatId :: Update -> Int
getChatId = chatId . getChat . getMessage

getText :: Update -> Maybe T.Text
getText u = getMessage u >>= messageText

getBodyResponse ::  LbsResponse -> HttpResponseBody LbsResponse
getBodyResponse  = responseBody

makeUpdateRequest :: Integer -> Req LbsResponse
makeUpdateRequest offset = req POST
                        (https api /: botToken /: "getUpdates")
                        NoReqBody
                        lbsResponse $
                        "timeout" =: (30000 :: Int) <>
                        "offset"  =: offset <>
                        responseTimeout 60000000


sendMessage :: Update -> MessageToSend -> Req LbsResponse
sendMessage update message = req POST
                                 (https api /: botToken /: "sendMessage")
                                 (ReqBodyJson message)
                                 lbsResponse
                                 mempty

run ::  Integer -> Req LbsResponse
run  offset = do
  response <- makeUpdateRequest offset
  let status = responseStatusCode response
  let body = responseBody response
  let lastUpdate = getLastUpdate =<< decode body
  liftIO $ print offset
  case lastUpdate of
   Nothing -> run offset
   _ -> do
     let message = MessageToSend { chat_id = fromJust $ getChatId <$> lastUpdate
                                , text    = fromJust $ getText <$> fromJust $ lastUpdate }
     sendMessage (fromJust lastUpdate) message
     run ( offset +1)


main :: IO ()
main = do   
  response <- runReq defaultHttpConfig $ makeUpdateRequest 0
  let body = responseBody response
  let oldUpdate = getLastUpdate =<< decode body
  let offset = getOffset oldUpdate 
  runReq defaultHttpConfig $ run offset
  return ()

