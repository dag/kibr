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
    parts <- Irc.parts nullBotConf st
    env <- mkBotEnv c ps
    forM_ parts $ \p -> runBotPartT (msum [p, return ()]) env
    return $ outChan env

case_join_command :: Assertion
case_join_command =
  do
    chan <- receive "PRIVMSG" ["#channel", "@join #other-channel"]
    msg  <- readChan chan
    msg @?= Message Nothing "JOIN" ["#other-channel"]

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

case_affix_lookup :: Assertion
case_affix_lookup =
  do
    chan <- receive "PRIVMSG" ["#channel", "@affix dohi"]
    Message{..} <- readChan chan
    map (take 28) msg_params @?= [ "#channel"
                                 , "Just (Word {_word = \"donri\","
                                 ]
