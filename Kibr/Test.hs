{-# OPTIONS_GHC -O0 -F -pgmF htfpp #-}

module Kibr.Test where

import Data.Map as Map
import Data.Set as Set
import System.Exit (exitWith)

import Test.Framework hiding (runTest)

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
fixtures =
  let
    def d n = Map.fromList [(English, rev d n)]
    rev d n = [Revision (Definition d n) (Just "Imported")]
  in
    Dictionary $ Set.fromList
        [ Word "ba'e" (Particle [] False BAhE) $
            def "forethought emphasis indicator." Nothing
        ]
