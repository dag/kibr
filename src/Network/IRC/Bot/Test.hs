module Network.IRC.Bot.Test where

import Preamble

import Control.Concurrent.Chan

import Network.IRC
import Network.IRC.Bot

mkBotEnv :: Command -> [Parameter] -> IO BotEnv
mkBotEnv c ps =
  do
    chan <- newChan
    return BotEnv { message   = msg
                  , outChan   = chan
                  , logFn     = nullLogger
                  , botName   = "bot"
                  , cmdPrefix = "@"
                  }
  where
    prefix = NickName "user" (Just "user") (Just "localhost")
    msg    = Message { msg_prefix  = Just prefix
                     , msg_command = c
                     , msg_params  = ps
                     }
