{-# OPTIONS_GHC -F -pgmF trhsx #-}
{-# LANGUAGE OverloadedStrings #-}

module Kibr.Fixture
    ( Fixture(..)
    , fixtures
    , dictionary
    , XML
    , ba'e
    , la'oi
    , donri
    , kibro
    , jbobau
    , pinpedi
    , sferies
    , ba'unai
    )
  where

import Data.Packable
import Kibr.Data
import Text.XML.HXT.Core
import Text.XML.HXT.HSX

type XML = IOSArrow XmlTree XmlTree

data Fixture = Fixture
    { fWord           :: Word
    , fWordType       :: WordType
    , fWordDefinition :: WordDefinition
    , fXML            :: XML
    }

fixtures :: [Fixture]
fixtures =
    [ ba'e
    , la'oi
    , donri
    , kibro
    , jbobau
    , pinpedi
    , sferies
    , ba'unai
    ]

dictionary :: XML
dictionary =
    <dictionary>
      <direction from="lojban" to="English">
        <% fXML ba'e %>
      </direction>
    </dictionary>

ba'e :: Fixture
ba'e = Fixture
    { fWord = "ba'e"
    , fWordType = ParticleWord (fromList []) "BAhE"
    , fWordDefinition = "forethought emphasis indicator."
    , fXML =
        <valsi word="ba'e" type="cmavo">
          <selmaho>BAhE</selmaho>
          <definition>forethought emphasis indicator.</definition>
        </valsi>
    }

la'oi :: Fixture
la'oi = Fixture
    { fWord = "la'oi"
    , fWordType = ParticleWord (fromList []) ExperimentalParticle
    , fWordDefinition = WordDefinition "single-word non-Lojban name."
                                $ Just "See also {la'o}, {zo'oi}."
    , fXML =
        <valsi unofficial="true" word="la'oi" type="experimental cmavo">
          <selmaho>ZOhOI</selmaho>
          <definition>single-word non-Lojban name.</definition>
          <notes>See also {la'o}, {zo'oi}.</notes>
        </valsi>
    }

donri :: Fixture
donri = Fixture
    { fWord = "donri"
    , fWordType = RootWord (fromList ["dor", "do'i"]) OfficialRoot
    , fWordDefinition = WordDefinition "$x_{1}$ is the daytime of day $x_{2}$."
                                $ Just "See also {nicte}, {djedi}, {tcika}."
    , fXML =
        <valsi word="donri" type="gismu">
          <rafsi>dor</rafsi>
          <rafsi>do'i</rafsi>
          <definition>$x_{1}$ is the daytime of day $x_{2}$.</definition>
          <notes>See also {nicte}, {djedi}, {tcika}.</notes>
        </valsi>
    }

kibro :: Fixture
kibro = Fixture
    { fWord = "kibro"
    , fWordType = RootWord (fromList []) ExperimentalRoot
    , fWordDefinition = "$x_1$ pertains to the internet in aspect $x_2$."
    , fXML =
        <valsi unofficial="true" word="kibro" type="experimental gismu">
          <definition>$x_1$ pertains to the internet in aspect $x_2$.</definition>
        </valsi>
    }

jbobau :: Fixture
jbobau = Fixture
    { fWord = "jbobau"
    , fWordType = CompoundWord
    , fWordDefinition = "$l_1=b_1$ is Lojban used by $b_2$."
    , fXML =
        <valsi word="jbobau" type="lujvo">
          <definition>$l_1=b_1$ is Lojban used by $b_2$.</definition>
        </valsi>
    }

pinpedi :: Fixture
pinpedi = Fixture
    { fWord = "pinpedi"
    , fWordType = LoanWord
    , fWordDefinition = "$x_1$ is a seal of species $x_2$."
    , fXML =
        <valsi word="pinpedi" type="fu'ivla">
          <definition>$x_1$ is a seal of species $x_2$.</definition>
        </valsi>
    }

sferies :: Fixture
sferies = Fixture
    { fWord = "sferies"
    , fWordType = NameWord
    , fWordDefinition = "Sweden."
    , fXML =
        <valsi word="sferies" type="cmene">
          <definition>Sweden.</definition>
        </valsi>
    }

ba'unai :: Fixture
ba'unai = Fixture
    { fWord = "ba'unai"
    , fWordType = ParticleCluster
    , fWordDefinition = "discursive: understatement."
    , fXML =
        <valsi word="ba'unai" type="cmavo cluster">
          <selmaho>UI*3</selmaho>
          <definition>discursive: understatement.</definition>
        </valsi>
    }
