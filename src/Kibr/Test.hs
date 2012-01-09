{-# OPTIONS_GHC -O0 -F -pgmF htfpp #-}

module Kibr.Test where

import Preamble

import Data.Map as Map
import Data.Set as Set
import Data.IxSet as Ix
import System.Exit (exitWith)
import System.IO (IO)

import Test.Framework

import Kibr.Data
import Kibr.Data.State
import Kibr.Xml (readDictionary)

run :: [String] -> IO ()
run args = exitWith =<< runTestWithArgs args allHTFTests

test_fixtures :: IO ()
test_fixtures =
  do
    dictionary <- readDictionary English "fixtures.xml"
    assertEqual dictionary fixtures

fixtures :: State
fixtures =
  let
    def d n = Map.fromList [(English, rev d n)]
    rev d n = [Revision (Definition d n) (Just "Imported")]
  in
    State $ Ix.fromList
      [ Word "ba'e" (Particle Set.empty False BAhE) $
          def "forethought emphasis indicator." Nothing
      , Word "la'oi" (Particle Set.empty True Undefined) .
          def "single-word non-Lojban name." $
            Just "See also {la'o}, {zo'oi}."
      , Word "donri" (Root (Set.fromList ["dor", "do'i"]) False) .
          def "$x_{1}$ is the daytime of day $x_{2}$." $
            Just "See also {nicte}, {djedi}, {tcika}."
      , Word "kibro" (Root Set.empty True) $
          def "$x_1$ pertains to the internet in aspect $x_2$." Nothing
      , Word "jbobau" Compound $
          def "$l_1=b_1$ is Lojban used by $b_2$." Nothing
      , Word "pinpedi" Loan $
          def "$x_1$ is a seal of species $x_2$." Nothing
      , Word "sferies" Name $ def "Sweden." Nothing
      , Word "ba'unai" Cluster $ def "discursive: understatement." Nothing
      ]
