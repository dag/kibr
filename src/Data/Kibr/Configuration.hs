module Data.Kibr.Configuration where

import Happstack.Server (Conf, nullConf)
import Network.IRC.Bot  (BotConf(..), User(..), nullBotConf, nullUser)

import qualified Data.Set as Set

data Configuration
  = Configuration
      { http :: Conf
      , irc  :: BotConf
      }

master :: Configuration
master =
  Configuration
    { http = nullConf
    , irc  = nullBotConf
               { host = "irc.freenode.net"
               , nick = "kibr"
               , user = nullUser { username = "kibr"
                                 , realname = "kibr bot"
                                 }
               , commandPrefix = "@"
               , channels      = Set.fromList ["#sampla"]
               }
    }
