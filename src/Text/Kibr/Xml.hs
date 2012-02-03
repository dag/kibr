{-# LANGUAGE Arrows #-}

module Text.Kibr.Xml where

import Preamble
import Prelude (error)

import Data.Acid
import Data.Kibr.State hiding (words)
import Data.Kibr.Language
import Text.XML.HXT.Core

import qualified Data.HiggsSet      as Higgs
import qualified Data.Kibr.Grammar  as DB
import qualified Data.Kibr.Revision as DB
import qualified Data.Kibr.Word     as DB
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Data.Text          as T

run :: [String] -> Acid -> IO ()
run (file:_) state =
  do
    db    <- readDictionary English file
    update state $ WriteState db
run _ _ = error "usage: kibr import <file>"

readDictionary :: Language -> String -> IO State
readDictionary language file =
  do
    words <- runX $ readDocument [] file >>> deep (getWord language)
    pure . State $ Higgs.fromList words

getWord :: ArrowXml a => Language -> a XmlTree DB.Word
getWord language = hasName "valsi" >>> proc valsi ->
  do
    word       <- getAttrValue "word"              -< valsi
    type_      <- getAttrValue "type"              -< valsi
    definition <- getElemText "definition"         -< valsi
    notes      <- getMaybe $ getElemText "notes"   -< valsi
    rafsi      <- listA    $ getElemText "rafsi"   -< valsi
    selma'o    <- getMaybe $ getElemText "selmaho" -< valsi

    let affixes     = Set.fromList $ T.pack <$> rafsi
        shape       = getShape type_ affixes grammar
        grammar     = getGrammar selma'o word
        revision    = DB.Revision definition' $ Just "Imported"
        definition' = DB.Definition (T.pack definition) $ T.pack <$> notes
        definitions = Map.fromList [(language, [revision])]

    returnA -< DB.Word (T.pack word) shape definitions

getShape :: String -> Set Text -> DB.Grammar -> DB.Shape
getShape type_ rafsi grammar =
  case type_ of
    "cmavo"              -> DB.Particle rafsi grammar
    "experimental cmavo" -> DB.ProposedParticle rafsi
    "gismu"              -> DB.Root rafsi False
    "experimental gismu" -> DB.Root rafsi True
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
