module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T
import           Data.Text.Lazy.Builder.Int     ( decimal )
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Maybe                     ( fromJust )
import qualified Text.URI as URI
import Telegram.Bot.API.Types
import BotConfig


data MyData = MyData
  { size  :: Int
  , color :: T.Text
  } deriving (Show, Generic)

instance ToJSON MyData
instance FromJSON MyData

api :: T.Text
api = "api.telegram.org"

botToken :: T.Text
botToken = mconcat ["bot", token]

getUpdates :: Req LbsResponse
getUpdates = req POST
             (https api /: botToken /: "getUpdates")
             NoReqBody
             lbsResponse
             mempty

getLastUpdate :: Updates -> Update
getLastUpdate  = last . result

getMessage :: Update -> Maybe Message
getMessage  =  message

getChat :: Maybe Message -> Chat
getChat (Just m) = messageChat m

getChatId :: Update -> Int
getChatId = chatId . getChat . getMessage

getText :: Update -> Maybe T.Text
getText u =  getMessage u >>=  messageText

data MessageToSend =  MessageToSend
  { chat_id  :: Int
  , text :: T.Text
  } deriving (Show, Generic)

instance ToJSON MessageToSend
instance FromJSON MessageToSend

sendMessage :: Update -> MessageToSend -> Req LbsResponse
sendMessage update message = req POST
             (https api /: botToken /: "sendMessage")
             (ReqBodyJson message)
             lbsResponse
             mempty

main :: IO ()
main = runReq defaultHttpConfig $ do
  response <- getUpdates
  let body = responseBody response
  let (Right update) = eitherDecode body :: Either String Updates
  let lastUpdate = getLastUpdate update
  let message = MessageToSend
        { chat_id = getChatId lastUpdate
        , text = fromJust $ getText lastUpdate }
  sendMessage lastUpdate message
  liftIO $ print $ lastUpdate
  -- return ()



