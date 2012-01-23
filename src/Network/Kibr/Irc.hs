module Network.Kibr.Irc where

import Preamble

import Control.Concurrent            (killThread)
import Data.Acid.Advanced            (query')
import Data.Kibr.State
import Data.Text                     (pack)
import Network.IRC.Bot
import Network.IRC.Bot.Part.Channels (initChannelsPart)
import Network.IRC.Bot.Part.NickUser (nickUserPart)
import Network.IRC.Bot.Part.Ping     (pingPart)
import System.IO                     (getLine)
import Text.Parsec

import qualified Data.Set as Set

run :: [String] -> Acid -> IO ()
run _ state =
  do
    (_, channelsPart) <- initChannelsPart . Set.fromList $ ["#sampla"]
    threads <- simpleBot conf [ pingPart
                              , nickUserPart
                              , channelsPart
                              , wordPart state
                              ]
    getLine
    mapM_ killThread threads
  where
    conf = nullBotConf { host = "irc.freenode.net"
                       , nick = "kibr"
                       , user = nullUser { username = "kibr"
                                         , realname = "kibr bot"
                                         }
                       }

wordPart :: BotMonad m => Acid -> m ()
wordPart state = parsecPart $ \target ->
  do
    char '!'
    word <- many1 . oneOf $ "abcdefghijklmnoprstuvxyz'."
    w <- query' state . LookupWord . pack $ word
    sendCommand $ PrivMsg Nothing [target] (show w)
  <|>
    pure ()
