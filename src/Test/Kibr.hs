module Test.Kibr where

import Preamble

import Data.IxSet as Ix
import Data.Map   as Map
import Data.Set   as Set
import System.Environment (withArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit hiding (State)
import Test.HUnit.Diff

import Data.Kibr.Grammar
import Data.Kibr.Language
import Data.Kibr.Revision
import Data.Kibr.State
import Data.Kibr.Word
import Language.CSS.YUI
import Text.Kibr.Xml (readDictionary)

import qualified Data.Text.Lazy as LT

run :: [String] -> IO ()
run args = withArgs args $defaultMainGenerator

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

case_readDictionary :: Assertion
case_readDictionary =
  do
    dictionary <- readDictionary English "fixtures.xml"
    dictionary @?== fixtures

case_pxToPercent_computed :: Assertion
case_pxToPercent_computed =
  do
    pxToPercent 9  @?== "69.2%"
    pxToPercent 28 @?== "215.4%"

prop_pxToPercent_starts_with_digit :: Int -> Bool
prop_pxToPercent_starts_with_digit px
  | px >= 0   = isDigit . LT.head . pxToPercent $ px
  | otherwise = LT.head (pxToPercent px) == '-'

prop_pxToPercent_ends_in_percent_sign :: Int -> Bool
prop_pxToPercent_ends_in_percent_sign px = LT.last (pxToPercent px) == '%'
