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


prop_getWord :: String -> [String] -> Bool -> String -> Maybe String -> Bool
prop_getWord name' affixes' experimental' definition' notes' =
    runLA (input >>> getWord) [] == [word]
  where
    word  = Word { name       = name'
                 , shape      = Root { affixes      = affixes'
                                     , experimental = experimental'
                                     }
                 , definition = definition'
                 , notes      = notes'
                 }
    input = mkelem "valsi" [ sattr "word" name'
                           , sattr "type" $ if experimental'
                                               then "experimental gismu"
                                               else "gismu"
                           ]
                $ selem "definition" [txt definition'] :
                [ selem "rafsi" [txt a] | a <- affixes' ] ++
                  case notes' of
                       Just n  -> [ selem "notes" [txt n] ]
                       Nothing -> []


test_getWord :: IO ()
test_getWord =
  do
    result <- runX $ readString [] input >>> getWord
    assertEqual [word] result
  where
    word        = Word { name       = "kibro"
                       , shape      = Root { affixes = [], experimental = True }
                       , definition = definition'
                       , notes      = Just notes'
                       }
    definition' = "$x_1$ pertains to the internet/cyberspace in aspect $x_2$."
    notes'      = "Proposed by xorxes. Short rafsi -kib-. Cf. {mujysamseltcana}."
    input       = concat [ "<valsi unofficial='true' "
                         , "word='kibro' type='experimental gismu'>"
                         , "<definition>", definition', "</definition>"
                         , "<notes>", notes', "</notes>"
                         , "</valsi>"
                         ]
