module Test.Kibr.Fixture (fixtures) where

import Preamble

import Data.Kibr.Grammar
import Data.Kibr.Language
import Data.Kibr.Revision
import Data.Kibr.State
import Data.Kibr.Word

import qualified Data.IxSet as Ix
import qualified Data.Map   as Map
import qualified Data.Set   as Set

define :: Text -> Maybe Text -> Map.Map Language [Revision Definition]
define d n =
    Map.fromList [(English, rev)]
  where
    rev = [Revision (Definition d n) (Just "Imported")]

fixtures :: State
fixtures = State $ Ix.fromList
    [ Word "ba'e" (Particle Set.empty BAhE) $
        define "forethought emphasis indicator." Nothing
    , Word "la'oi" (ProposedParticle Set.empty) .
        define "single-word non-Lojban name." $
          Just "See also {la'o}, {zo'oi}."
    , Word "donri" (Root (Set.fromList ["dor", "do'i"]) False) .
        define "$x_{1}$ is the daytime of day $x_{2}$." $
          Just "See also {nicte}, {djedi}, {tcika}."
    , Word "kibro" (Root Set.empty True) $
        define "$x_1$ pertains to the internet in aspect $x_2$." Nothing
    , Word "jbobau" Compound $
        define "$l_1=b_1$ is Lojban used by $b_2$." Nothing
    , Word "pinpedi" Loan $
        define "$x_1$ is a seal of species $x_2$." Nothing
    , Word "sferies" Name $ define "Sweden." Nothing
    , Word "ba'unai" Cluster $ define "discursive: understatement." Nothing
    ]
