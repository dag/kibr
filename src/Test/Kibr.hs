module Test.Kibr where

import Preamble

import System.Environment (withArgs)
import Test.Framework     (defaultMain)

import qualified Test.Kibr.Css  as Css
import qualified Test.Kibr.Http as Http
import qualified Test.Kibr.Xml  as Xml

run :: [String] -> IO ()
run args = withArgs args $ defaultMain [Css.tests, Http.tests, Xml.tests]
