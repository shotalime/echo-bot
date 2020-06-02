module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
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


           

main :: IO ()
main = runReq defaultHttpConfig $ do
  response <- getUpdates
  let body = responseBody response 
  let dec = eitherDecode body :: Either String Updates
  liftIO $ print  dec
  -- return ()
  


