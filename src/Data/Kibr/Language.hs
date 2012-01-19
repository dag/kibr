module Data.Kibr.Language where

import Preamble

import Data.SafeCopy

data Language
  = Lojban
  | English
  deriving (Eq, Show, Ord, Data, Typeable)

deriveSafeCopy 0 'base ''Language
