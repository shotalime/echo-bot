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


data MyData = MyData
  { size  :: Int
  , color :: T.Text
  } deriving (Show, Generic)

instance ToJSON MyData
instance FromJSON MyData

api :: T.Text
api = "api.telegram.org"

token :: T.Text
token = "1214858885:AAEa4sxstviWYUSkw4ELNpJE6ZAGLnQ_g-A"

botToken :: T.Text
botToken = mconcat ["bot", token]


getUpdates :: Req (JsonResponse Value)
getUpdates = req POST 
             (https api /: botToken /: "getUpdates")
             NoReqBody
             jsonResponse
             mempty


           

main :: IO ()
main = runReq defaultHttpConfig $ do
  response <- getUpdates
  let jsonBody = responseBody response
  -- L.writeFile "data.json" (responseBody v)
  liftIO $ L.writeFile "data.json" (encode jsonBody)


