module Data.Kibr.Message where

import Preamble

import Data.Kibr.Language

data Message
  = LojbanDictionary
  deriving (Eq, Show)

message :: Language -> Message -> Text

message English m = case m of
  LojbanDictionary -> "Lojban Dictionary"

message Lojban m = case m of
  LojbanDictionary -> "vlaste fu la lojban"
