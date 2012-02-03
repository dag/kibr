module Data.Kibr.Language where

import Preamble

import Data.SafeCopy
import Web.Routes.TH (derivePathInfo)

data Language
  = Lojban
  | English
  deriving (Eq, Show, Ord, Enum, Bounded, Data, Typeable)

derivePathInfo ''Language

deriveSafeCopy 0 'base ''Language
