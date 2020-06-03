module Main where

import Control.Monad
import Control.Monad.IO.Class
import           Control.Concurrent
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Maybe                     ( fromJust )
import qualified Text.URI as URI
import Telegram.Bot.API.Types
import BotConfig
import System.Timeout


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


getLastUpdate :: Updates -> Update
getLastUpdate  = last . result

getMessage :: Update -> Maybe Message
getMessage  =  updateMessage

getChat :: Maybe Message -> Chat
getChat (Just m) = messageChat m

getChatId :: Update -> Int
getChatId = chatId . getChat . getMessage

getText :: Update -> Maybe T.Text
getText u =  getMessage u >>=  messageText

getUpdateId :: Update -> Integer
getUpdateId = updateId

isNewUpdate :: Update -> Update -> Bool
isNewUpdate u1 u2 = getUpdateId u1 /= getUpdateId u2

data MessageToSend =  MessageToSend
  { chat_id  :: Int
  , text :: T.Text
  } deriving (Show, Generic)

instance ToJSON MessageToSend
instance FromJSON MessageToSend

getUpdates :: Req LbsResponse
getUpdates = req POST
             (https api /: botToken /: "getUpdates")
             NoReqBody
             lbsResponse
             mempty

sendMessage :: Update -> MessageToSend -> Req LbsResponse
sendMessage update message = req POST
             (https api /: botToken /: "sendMessage")
             (ReqBodyJson message)
             lbsResponse
             mempty

defUpdate :: Update
defUpdate = Update (-1)  Nothing Nothing Nothing Nothing


runTelegramBot :: Update -> IO ()
runTelegramBot oldUpdate = runReq defaultHttpConfig $ do
  liftIO $ timeout 1000000 (threadDelay 1000 *> pure "tick")
  response <- getUpdates
  let body = responseBody response
  let (Right update) = eitherDecode body :: Either String Updates
  let lastUpdate = getLastUpdate update
  if isNewUpdate lastUpdate oldUpdate  
    then do
      let message = MessageToSend
            { chat_id = getChatId lastUpdate
            , text = fromJust $ getText lastUpdate }
      sendMessage lastUpdate message
      liftIO $ print "send"  
    else liftIO $ print "no message"
  liftIO $ runTelegramBot lastUpdate
      
  

main :: IO ()
main = 
  runReq defaultHttpConfig $ do
  response <- getUpdates
  let body = responseBody response
  let (Right update) = eitherDecode body :: Either String Updates
  let oldUpdate = getLastUpdate update -- get  old update
  liftIO $ runTelegramBot oldUpdate 
  -- return ()



