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
prop_getWord wordName shapeAffixes shapeExperimental wordDef wordNote =
    runLA (input >>> getWord) [] == [word]
  where
    word  = Word { name = wordName
                 , shape = Root { affixes = shapeAffixes
                                , experimental = shapeExperimental
                                }
                 , definition = wordDef
                 , notes = wordNote
                 }
    input = mkelem "valsi" [ sattr "word" wordName
                           , sattr "type" (if shapeExperimental
                                             then "experimental gismu"
                                             else "gismu") ]
                (selem "definition" [txt wordDef] :
                 [ selem "rafsi" [txt a] | a <- shapeAffixes ] ++
                 case wordNote of
                      Just n -> [ selem "notes" [txt n] ]
                      Nothing -> [])


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
