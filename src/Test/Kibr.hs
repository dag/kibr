module Test.Kibr where

import Preamble

import Test.Framework (defaultMainWithArgs)

import qualified Test.Kibr.Css  as Css
import qualified Test.Kibr.Http as Http
import qualified Test.Kibr.Irc  as Irc
import qualified Test.Kibr.Xml  as Xml

run :: [String] -> IO ()
run = defaultMainWithArgs [Css.tests, Http.tests, Irc.tests, Xml.tests]
