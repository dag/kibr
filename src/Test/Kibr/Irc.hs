module Test.Kibr.Irc where

import Preamble

import Control.Concurrent.Chan        (Chan, readChan)
import Data.Acid.Memory               (openMemoryState)
import Data.List                      (take)
import Network.IRC
import Network.IRC.Bot
import Network.IRC.Bot.Test           (mkBotEnv)
import Test.Framework                 (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.TH              (testGroupGenerator)
import Test.HUnit                     (Assertion, (@?=))
import Test.Kibr.Fixture

import qualified Network.Kibr.Irc as Irc

tests :: Test
tests = $testGroupGenerator

receive :: Command -> [Parameter] -> IO (Chan Message)
receive c ps =
  do
    st <- openMemoryState fixtures
    parts <- Irc.parts st
    env <- mkBotEnv c ps
    forM_ parts $ \p -> runBotPartT (p `mplus` pure ()) env
    pure $ outChan env

case_word_lookup :: Assertion
case_word_lookup =
  do
    chan <- receive "PRIVMSG" ["#channel", "!bahunai"]
    Message{..} <- readChan chan
    msg_prefix @?= Nothing
    msg_command @?= "PRIVMSG"
    map (take 30) msg_params @?= [ "#channel"
                                 , "Just (Word {_word = \"ba'unai\","
                                 ]
