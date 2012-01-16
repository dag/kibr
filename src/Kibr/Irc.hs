module Kibr.Irc where

import Preamble

import Control.Concurrent (killThread)
import Kibr.Data.State
import Network.IRC.Bot
import Network.IRC.Bot.Part.Ping
import System.IO          (IO, getLine)

run :: [String] -> Acid -> IO ()
run _ _ =
  do
    threads <- simpleBot conf [pingPart]
    _ <- getLine
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
