{-# LANGUAGE Arrows #-}

module Kibr.Xml where

import Kibr.Data (Word (Word)
                 ,Shape (Particle,Root,Compound,Loan,Name,Cluster)
                 )

import Data.Maybe
import Text.XML.HXT.Core


getElemText :: ArrowXml a => String -> a XmlTree String
getElemText name =
    getChildren  >>>
    hasName name >>>
    getChildren  >>>
    getText


getMaybe :: ArrowIf a => a b c -> a b (Maybe c)
getMaybe a = (a >>> arr Just) `orElse` constA Nothing


getWord :: ArrowXml a => a XmlTree Word
getWord = deep $ hasName "valsi" >>>
    proc valsi -> do
        word       <- getAttrValue "word"              -< valsi
        type_      <- getAttrValue "type"              -< valsi
        definition <- getElemText "definition"         -< valsi
        notes      <- getMaybe $ getElemText "notes"   -< valsi
        rafsi      <- listA    $ getElemText "rafsi"   -< valsi
        selma'o    <- getMaybe $ getElemText "selmaho" -< valsi

        let grammar = fromMaybe "" selma'o
            shape   =
              case type_ of
                  "cmavo"              -> Particle rafsi False grammar
                  "experimental cmavo" -> Particle rafsi True  grammar
                  "gismu"              -> Root     rafsi False
                  "experimental gismu" -> Root     rafsi True
                  "lujvo"              -> Compound
                  "fu'ivla"            -> Loan
                  "cmene"              -> Name
                  "cmavo cluster"      -> Cluster
                  _                    -> error $ "unrecognized word type "
                                                  ++ show type_

        returnA -< Word word shape definition notes


mkDb :: String -> IO [Word]
mkDb file = runX $ readDocument [] file >>> getWord
