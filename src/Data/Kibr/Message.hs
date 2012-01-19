module Data.Kibr.Message where

import Preamble

import Data.Kibr.Language

data Message
  = LojbanDictionary
  deriving (Eq, Show)

msg :: Language -> Message -> Text

msg English m = case m of
  LojbanDictionary -> "Lojban Dictionary"

msg Lojban m = case m of
  LojbanDictionary -> "vlaste fu la lojban"
