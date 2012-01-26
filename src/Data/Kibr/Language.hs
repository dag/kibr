module Data.Kibr.Language where

import Preamble

import Data.SafeCopy

data Language
  = Lojban
  | English
  deriving (Eq, Show, Ord, Enum, Bounded, Data, Typeable)

deriveSafeCopy 0 'base ''Language
