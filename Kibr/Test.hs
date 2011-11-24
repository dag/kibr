{-# OPTIONS_GHC -O0 -F -pgmF htfpp #-}

module Kibr.Test where

import Data.Map as Map
import System.Exit (exitWith)

import Test.Framework hiding (runTest)
import Text.XML.HXT.Core

import Kibr.Data
import Kibr.Xml (readDictionary)


runTest :: [String] -> IO ()
runTest args =
    exitWith =<< runTestWithArgs args allHTFTests


test_fixtures :: IO ()
test_fixtures =
  do
    dictionary <- readDictionary English "fixtures.xml"
    assertEqual dictionary fixtures


fixtures :: Dictionary
fixtures = Map.fromList
    [ ("ba'e", Word (Particle [] False BAhE) $ Map.fromList
        [(English, Definition "forethought emphasis indicator." Nothing)])
    ]
