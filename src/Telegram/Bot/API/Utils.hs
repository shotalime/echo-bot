module Telegram.Bot.API.Utils where
  
import           Text.Casing

toTelegramName :: String -> String -> String
toTelegramName str = quietSnake . drop (length str)