module Kibr.Irc where

import Preamble

import Network.IRC.Bot
import Network.IRC.Bot.Part.Ping

run :: [String] -> IO ()
run _ = 
  do
    simpleBot conf [pingPart]
    return ()
  where
    conf = nullBotConf { host = "irc.freenode.net"
                       , nick = "kibr"
                       , channel = "#sampla"
                       }
