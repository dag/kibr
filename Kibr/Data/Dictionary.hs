{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Kibr.Data.Dictionary where

import Kibr.Data.Grammar
import Kibr.Data.Language
import Kibr.Data.Revision

import Data.Data
import Data.Map
import Data.SafeCopy
import Data.Set

data Shape
  = Particle
      { affixes      :: Set String
      , experimental :: Bool
      , grammar      :: Grammar
      }
  | Root
      { affixes      :: Set String
      , experimental :: Bool
      }
  | Compound
  | Loan
  | Name
  | Cluster
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Shape

data Definition
  = Definition
      { definition :: String
      , notes      :: Maybe String
      }
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Definition

data Word
  = Word
      { word        :: String
      , shape       :: Shape
      , definitions :: Map Language [Revision Definition]
      }
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Word

data Dictionary
  = Dictionary { words :: Set Word }
    deriving (Eq, Show, Data, Typeable)
deriveSafeCopy 0 'base ''Dictionary

empty :: Dictionary
empty = Dictionary Data.Set.empty
