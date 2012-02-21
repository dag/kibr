module Data.Kibr.Configuration where

import Preamble

import System.Console.GetOpt

import qualified Data.Set         as Set
import qualified Happstack.Server as Http
import qualified Network.IRC.Bot  as Irc

data Configuration
  = Configuration
      { http :: Http.Conf
      , irc  :: Irc.BotConf
      }

master :: Configuration
master =
  Configuration
    { http = Http.nullConf
    , irc  = Irc.nullBotConf
               { Irc.host = "irc.freenode.net"
               , Irc.nick = "kibr"
               , Irc.user = Irc.nullUser { Irc.username = "kibr"
                                         , Irc.realname = "kibr bot"
                                         }
               , Irc.commandPrefix = "@"
               , Irc.channels      = Set.fromList ["#sampla"]
               }
    }

options :: [OptDescr (Configuration -> Configuration)]
options =
  [ Option [] ["http-port"]
      (ReqArg (\a c -> c { http = (http c) { Http.port = read a } }) "NUM")
      "Port to listen for HTTP requests on."
  ]
