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


main :: IO ()
main = do   
  response <- runReq defaultHttpConfig $ makeUpdateRequest 0
  let body = responseBody response
  let oldUpdate = getFirstUpdate  =<< decode body
  let offset = getOffset oldUpdate 
  runReq defaultHttpConfig $ run offset
  return ()

