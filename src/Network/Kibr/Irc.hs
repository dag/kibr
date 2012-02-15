module Network.Kibr.Irc where

import Preamble hiding (join)

import Control.Concurrent            (killThread)
import Control.Concurrent.STM        (TVar)
import Data.Acid.Advanced            (query')
import Data.Kibr.State
import Data.Text                     (pack)
import Network.IRC.Bot
import Network.IRC.Bot.Part.Channels (initChannelsPart, joinChannel)
import Network.IRC.Bot.Part.NickUser (nickUserPart)
import Network.IRC.Bot.Part.Ping     (pingPart)
import System.IO                     (getLine)
import Text.Parsec

import qualified Data.Set as Set

run :: [String] -> Acid -> IO ()
run _ state =
  do
    ps <- parts state
    threads <- simpleBot conf ps
    getLine
    mapM_ killThread threads
  where
    conf = nullBotConf { host = "irc.freenode.net"
                       , nick = "kibr"
                       , user = nullUser { username = "kibr"
                                         , realname = "kibr bot"
                                         }
                       , commandPrefix = "@"
                       }

parts :: Acid -> IO [BotPartT IO ()]
parts state =
  do
    (channels, channelsPart) <- initChannelsPart . Set.fromList $ ["#sampla"]
    pure [ pingPart
         , nickUserPart
         , channelsPart
         , join channels
         , word state
         ]

join :: BotMonad m => TVar (Set String) -> m ()
join channels = parsecPart $
  do
    botPrefix
    string "join"
    space
    spaces
    channel <- many1 anyChar
    joinChannel channel channels
  <|>
    pure ()

word :: BotMonad m => Acid -> m ()
word state = parsecPart $
  do
    char '!'
    w <- many1 . oneOf $ "abcdefghijklmnoprstuvxyz'."
    w' <- query' state . LookupWord . pack $ w
    target <- maybeZero =<< replyTo
    sendCommand $ PrivMsg Nothing [target] (show w')
  <|>
    pure ()
