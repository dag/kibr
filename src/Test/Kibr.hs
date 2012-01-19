module Test.Kibr where

import Preamble

import Data.Algorithm.Diff
import Data.IxSet as Ix
import Data.Map   as Map
import Data.Set   as Set
import Prelude (lines, unlines)
import System.Console.ANSI
import System.Environment (withArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit hiding (State)
import Text.Groom

import Data.Kibr.Grammar
import Data.Kibr.Language
import Data.Kibr.Revision
import Data.Kibr.State
import Data.Kibr.Word
import Text.Kibr.Xml (readDictionary)

(@?==) :: (Eq a, Show a) => a -> a -> Assertion
x @?== y =
    assertBool ('\n':msg) (x == y)
  where
    msg       = unlines $ fmt <$> getDiff (lines . groom $ x)
                                          (lines . groom $ y)
    fmt (B,s) =               ' ' : s
    fmt (F,s) = color Green $ '+' : s
    fmt (S,s) = color Red   $ '-' : s
    color c s = setSGRCode [SetColor Foreground Dull c]
                ++ s ++
                setSGRCode [Reset]

run :: [String] -> IO ()
run args = withArgs args $defaultMainGenerator

case_fixtures :: Assertion
case_fixtures =
  do
    dictionary <- readDictionary English "fixtures.xml"
    dictionary @?== fixtures

fixtures :: State
fixtures =
  let
    def d n = Map.fromList [(English, rev d n)]
    rev d n = [Revision (Definition d n) (Just "Imported")]
  in
    State $ Ix.fromList
      [ Word "ba'e" (Particle Set.empty BAhE) $
          def "forethought emphasis indicator." Nothing
      , Word "la'oi" (ProposedParticle Set.empty) .
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
