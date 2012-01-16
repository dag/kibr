module Kibr.Irc where

import Preamble

import Control.Concurrent (killThread)
import System.IO          (IO, getLine)
import Network.IRC.Bot
import Network.IRC.Bot.Part.Ping

run :: [String] -> IO ()
run _ = 
  do
    threads <- simpleBot conf [pingPart]
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
