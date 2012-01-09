{-# LANGUAGE Arrows #-}

module Kibr.Xml where

import Preamble
import Prelude (error)

import Data.Acid
import Kibr.Data.State
import Text.XML.HXT.Core

import qualified System.IO as IO

import qualified Data.IxSet as Ix
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Kibr.Data  as DB

run :: [String] -> IO.IO ()
run (file:_) =
  do
    db    <- readDictionary DB.English file
    state <- openLocalState $ State Ix.empty
    update state $ WriteState db
    closeAcidState state

readDictionary :: DB.Language -> String -> IO.IO State
readDictionary language file =
  do
    words <- runX $ readDocument [] file >>> deep (getWord language)
    return . State $ Ix.fromList words

getWord :: ArrowXml a => DB.Language -> a XmlTree DB.Word
getWord language = hasName "valsi" >>> proc valsi ->
  do
    word       <- getAttrValue "word"              -< valsi
    type_      <- getAttrValue "type"              -< valsi
    definition <- getElemText "definition"         -< valsi
    notes      <- getMaybe $ getElemText "notes"   -< valsi
    rafsi      <- listA    $ getElemText "rafsi"   -< valsi
    selma'o    <- getMaybe $ getElemText "selmaho" -< valsi

    let affixes     = Set.fromList rafsi
        shape       = getShape type_ affixes grammar
        grammar     = getGrammar selma'o word
        revision    = DB.Revision definition' $ Just "Imported"
        definition' = DB.Definition definition notes
        definitions = Map.fromList [(language, [revision])]

    returnA -< DB.Word word shape definitions

getShape :: String -> Set.Set String -> DB.Grammar -> DB.Shape
getShape type_ rafsi grammar =
  case type_ of
    "cmavo"              -> DB.Particle rafsi False grammar
    "experimental cmavo" -> DB.Particle rafsi True  DB.Undefined
    "gismu"              -> DB.Root     rafsi False
    "experimental gismu" -> DB.Root     rafsi True
    "lujvo"              -> DB.Compound
    "fu'ivla"            -> DB.Loan
    "cmene"              -> DB.Name
    "cmavo cluster"      -> DB.Cluster
    _                    -> error $ "unrecognized word type " ++ show type_

getGrammar :: Maybe String -> String -> DB.Grammar
getGrammar selma'o word =
  case selma'o of
    Just a  -> read a
    Nothing -> error $ "particle without grammatical class: " ++ show word

getElemText :: ArrowXml a => String -> a XmlTree String
getElemText name = getChildren  >>> hasName name >>> getChildren  >>> getText

getMaybe :: ArrowIf a => a b c -> a b (Maybe c)
getMaybe a = (a >>> arr Just) `orElse` constA Nothing
