module Test.Kibr.Fixture (fixtures) where

import Preamble

import Data.Kibr.Grammar
import Data.Kibr.Language
import Data.Kibr.Revision
import Data.Kibr.State
import Data.Kibr.Word

import qualified Data.HiggsSet as Higgs
import qualified Data.Map      as Map
import qualified Data.Set      as Set

def :: Text -> Maybe Text -> Map.Map Language [Revision Definition]
def d n =
    Map.fromList [(English, rev)]
  where
    rev = [Revision (Definition d n) (Just "Imported")]

fixtures :: State
fixtures = State $ Higgs.fromList
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
