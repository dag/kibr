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

run :: BotConf -> Acid -> IO ()
run conf state =
  do
    ps <- parts conf state
    threads <- simpleBot conf ps
    getLine
    mapM_ killThread threads

parts :: BotConf -> Acid -> IO [BotPartT IO ()]
parts BotConf{..} state =
  do
    (chans, channelsPart) <- initChannelsPart channels
    pure [ pingPart
         , nickUserPart
         , channelsPart
         , join chans
         , word state
         , affix state
         ]

join :: BotMonad m => TVar (Set String) -> m ()
join chans = parsecPart $
  do
    try $ do
      botPrefix
      string "join"
    space
    spaces
    channel <- many1 anyChar
    joinChannel channel chans
  <|>
    pure ()

word :: BotMonad m => Acid -> m ()
word state = parsecPart $
  do
    char '!'
    spaces
    w <- many1 . oneOf $ "abcdefghijklmnoprstuvxyz'."
    eof
    w' <- query' state . LookupWord . pack $ w
    target <- maybeZero =<< replyTo
    sendCommand $ PrivMsg Nothing [target] (show w')
  <|>
    pure ()

affix :: BotMonad m => Acid -> m ()
affix state = parsecPart $
  do
    try $ do
      botPrefix
      string "affix"
    space
    spaces
    a <- many1 . oneOf $ "abcdefghijklmnoprstuvxyz'."
    eof
    w <- query' state . LookupAffix . pack $ a
    target <- maybeZero =<< replyTo
    sendCommand $ PrivMsg Nothing [target] (show w)
  <|>
    pure ()
