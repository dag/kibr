module Data.Kibr.Configuration where

import Preamble

import Data.Lens
import Data.Lens.Template
import System.Console.GetOpt

import qualified Data.Set         as Set
import qualified Happstack.Server as Http
import qualified Network.IRC.Bot  as Irc

data Http = Http { _port :: Int }

makeLens ''Http

data Irc
  = Irc
      { _server   :: String
      , _nickname :: String
      , _username :: String
      , _realname :: String
      , _prefix   :: String
      , _channels :: Set String
      }

makeLens ''Irc

data Configuration
  = Configuration
      { _http :: Http
      , _irc  :: Irc
      }

makeLens ''Configuration

master :: Configuration
master =
  Configuration
    { _http = Http { _port     = 8000 }
    , _irc  = Irc  { _server   = "irc.freenode.net"
                   , _nickname = "kibr"
                   , _username = "kibr"
                   , _realname = "kibr bot"
                   , _prefix   = "@"
                   , _channels = Set.fromList ["#sampla"]
                   }
    }

options :: [OptDescr (Configuration -> Configuration)]
options =
  [ Option [] ["http-port"]
      (ReqArg (\a -> port . http ^= read a) "NUM")
      "Port to listen for HTTP requests on."
  ]

toHappstackConf :: Configuration -> Http.Conf
toHappstackConf config =
  Http.nullConf { Http.port = config ^. port . http }

toBotConf :: Configuration -> Irc.BotConf
toBotConf config =
  Irc.nullBotConf
    { Irc.host = config ^. server . irc
    , Irc.nick = config ^. nickname . irc
    , Irc.user = Irc.nullUser
                   { Irc.username = config ^. username . irc
                   , Irc.realname = config ^. realname . irc
                   }
    , Irc.commandPrefix = config ^. prefix . irc
    , Irc.channels      = config ^. channels . irc
    }
