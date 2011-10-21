{-# OPTIONS_GHC -O0 -F -pgmF htfpp #-}

module Kibr.Test where

import System.Exit (exitWith)

import Test.Framework
import Text.XML.HXT.Core

import Kibr.Data
import Kibr.Xml (getWord)


runTest :: [String] -> IO ()
runTest args =
    exitWith =<< runTestWithArgs args allHTFTests


test_getWord :: IO ()
test_getWord =
  do
    [result] <- runX $ readString [] input >>> getWord
    assertEqual word result
  where
    word  = Word { name       = "kibro"
                 , shape      = Root { affixes = [], experimental = True }
                 , definition = defn
                 , notes      = Just note
                 }
    defn  = "$x_1$ pertains to the internet/cyberspace in aspect $x_2$."
    note  = "Proposed by xorxes. Short rafsi -kib-. Cf. {mujysamseltcana}."
    input = concat [ "<valsi unofficial='true' "
                   , "word='kibro' type='experimental gismu'>"
                   , "<definition>", defn, "</definition>"
                   , "<notes>", note, "</notes>"
                   , "</valsi>"
                   ]
