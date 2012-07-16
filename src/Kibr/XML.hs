{-# LANGUAGE Arrows, TypeOperators #-}

-- | Support for reading jbovlaste XML exports.
module Kibr.XML
    ( readDictionary
    , getWordType
    , getWordDefinition
    , getWord
    , getLanguage
    )
  where

import qualified Data.Set as Set

import Prelude hiding ((.))
import Control.Category ((.))

import Data.String   (fromString)
import Kibr.Data
import Text.XML.HXT.Core

getChild :: ArrowXml (~>) => String -> XmlTree ~> XmlTree
getChild name = hasName name . getChildren

getElemText :: ArrowXml (~>) => String -> XmlTree ~> String
getElemText name = getChild name /> getText

getMaybe :: ArrowXml (~>) => XmlTree ~> a -> XmlTree ~> Maybe a
getMaybe a = (Just ^<< a) `orElse` constA Nothing

-- | Parses a @\<valsi/>@ element into 'WordType'.
getWordType :: ArrowXml (~>) => XmlTree ~> WordType
getWordType = proc valsi -> do
    typeName <-            getAttrValue "type"    -< valsi
    rafsi    <- listA    $ getElemText  "rafsi"   -< valsi
    selmaho  <- getMaybe $ getElemText  "selmaho" -< valsi
    let affixForms = Set.fromList $ map fromString rafsi
        cls        = fmap fromString selmaho
    case typeName of
      "cmavo"   -> maybe zeroArrow (constA . ParticleWord affixForms) cls
      "experimental cmavo"
                -> constA $ ParticleWord affixForms ExperimentalParticle
      "gismu"   -> constA $ RootWord affixForms OfficialRoot
      "experimental gismu"
                -> constA $ RootWord affixForms ExperimentalRoot
      "lujvo"   -> constA   CompoundWord
      "fu'ivla" -> constA   LoanWord
      "cmene"   -> constA   NameWord
      "cmavo cluster"
                -> constA   ParticleCluster
      _         -> zeroArrow
      -<< ()

-- | Parses a @\<valsi/>@ element into 'WordDefinition'.
getWordDefinition :: ArrowXml (~>) => XmlTree ~> WordDefinition
getWordDefinition = proc valsi -> do
    def   <-            getElemText "definition" -< valsi
    notes <- getMaybe $ getElemText "notes"      -< valsi
    returnA -< WordDefinition (fromString def) $ fmap fromString notes

-- | Parses everything from a @\<valsi/>@ element.
getWord :: ArrowXml (~>) => XmlTree ~> (Word,WordType,WordDefinition)
getWord = proc valsi -> do
    word <- getAttrValue "word" -< valsi
    typ  <- getWordType         -< valsi
    def  <- getWordDefinition   -< valsi
    returnA -< (fromString word,typ,def)

-- | Parses the target language of a @\<direction/>@ element.
getLanguage :: ArrowXml (~>) => XmlTree ~> Language
getLanguage = proc dir -> do
    to <- getAttrValue "to" -< dir
    case to of
      "lojban"  -> constA   Lojban
      "English" -> constA $ English UnitedStates
      "Latina"  -> constA   Latin
      _         -> zeroArrow
      -<< ()

-- | Reads a complete @\<dictionary/>@ document, parsing both the target
-- 'Language' and every word from it.
readDictionary :: ArrowXml (~>)
               => XmlTree ~> (Language,[(Word,WordType,WordDefinition)])
readDictionary = proc dict -> do
    dir      <-         getChild "direction"              -< dict
    target   <-         hasAttrValue "from" (== "lojban") -< dir
    language <-         getLanguage                       -< target
    words    <- listA $ getWord <<< getChild "valsi"      -< target
    returnA -< (language,words)
