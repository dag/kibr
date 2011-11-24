{-# LANGUAGE Arrows #-}

module Kibr.Xml where

import Kibr.Data ( Word (Word)
                 , Shape (Particle, Root, Compound, Loan, Name, Cluster)
                 , Dictionary
                 , Language
                 , Grammar (Undefined)
                 , Definition (Definition)
                 )

import Data.Map as Map
import Text.XML.HXT.Core


readDictionary :: Language -> String -> IO Dictionary
readDictionary language file =
  do
    words <- runX $ readDocument [] file >>> getWord language
    return $ Map.fromList words


getWord :: ArrowXml a => Language -> a XmlTree (String, Word)
getWord language =
    deep $ hasName "valsi" >>> proc valsi ->
      do
        word       <- getAttrValue "word"              -< valsi
        type_      <- getAttrValue "type"              -< valsi
        definition <- getElemText "definition"         -< valsi
        notes      <- getMaybe $ getElemText "notes"   -< valsi
        rafsi      <- listA    $ getElemText "rafsi"   -< valsi
        selma'o    <- getMaybe $ getElemText "selmaho" -< valsi

        let shape       = getShape type_ rafsi grammar
            grammar     = getGrammar selma'o word
            definitions = Map.fromList [(language, Definition definition notes)]

        returnA -< (word, Word shape definitions)


getShape :: String -> [String] -> Grammar -> Shape
getShape type_ rafsi grammar =
  case type_ of
       "cmavo"              -> Particle rafsi False grammar
       "experimental cmavo" -> Particle rafsi True  Undefined
       "gismu"              -> Root     rafsi False
       "experimental gismu" -> Root     rafsi True
       "lujvo"              -> Compound
       "fu'ivla"            -> Loan
       "cmene"              -> Name
       "cmavo cluster"      -> Cluster
       _                    -> error $ "unrecognized word type " ++ show type_


getGrammar :: Maybe String -> String -> Grammar
getGrammar selma'o word =
  case selma'o of
       Just a  -> read a
       Nothing -> error $ "particle without grammatical class: " ++ show word


getElemText :: ArrowXml a => String -> a XmlTree String
getElemText name =
    getChildren  >>>
    hasName name >>>
    getChildren  >>>
    getText


getMaybe :: ArrowIf a => a b c -> a b (Maybe c)
getMaybe a = (a >>> arr Just) `orElse` constA Nothing
