module Network.Kibr.Irc where

import Preamble

import Control.Concurrent (killThread)
import Data.Acid.Advanced (query')
import Data.Kibr.State
import Data.Text          (pack)
import Network.IRC.Bot
import Network.IRC.Bot.Part.Ping
import System.IO          (getLine)
import Text.Parsec

run :: [String] -> Acid -> IO ()
run _ state =
  do
    threads <- simpleBot conf [pingPart, wordPart state]
    getLine
    mapM_ killThread threads
  where
    conf = nullBotConf { host = "irc.freenode.net"
                       , nick = "kibr"
                       , user = User { username = "kibr"
                                     , hostname = "."
                                     , servername = "."
                                     , realname = "kibr bot"
                                     }
                       , channel = "#sampla"
                       }

wordPart :: BotMonad m => Acid -> m ()
wordPart state = parsecPart $ \target ->
  do
    char '!'
    word <- many1 letter
    w <- query' state . LookupWord . pack $ word
    sendCommand $ PrivMsg Nothing [target] (show w)
  <|>
    pure ()
