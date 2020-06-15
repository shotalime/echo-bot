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
import           Data.Maybe                     ( fromJust, isJust )
import qualified Text.URI                      as URI
import           Telegram.Bot.API.Types
import           Telegram.Bot.API.Methods
import           BotConfig
import qualified Data.Configurator as C

main :: IO ()
main = do   
  conf <- C.load [C.Required "./src/Echo-bot.conf"]
  token <- C.lookup conf (T.pack "telegramConf.token") :: IO (Maybe T.Text)
  numOfRep <- C.lookup conf (T.pack "telegramConf.repeat") :: IO (Maybe Int)

  response <- runReq defaultHttpConfig $ makeUpdateRequest 0 (fromJust token)
  let updates = getUpdates response
  let oldUpdate = getFirstUpdate updates
  let offset = getOffset oldUpdate 
  runReq defaultHttpConfig $ run (fromJust token) offset (fromJust numOfRep)
  return ()

