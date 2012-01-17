{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Kibr.Data.Word where

import Preamble

import Kibr.Data.Grammar
import Kibr.Data.Language
import Kibr.Data.Revision

import Data.Data
import Data.IxSet as Ix
import Data.Lens.Template
import Data.Map
import Data.SafeCopy
import Data.Set

data Shape
  = Particle
      { _affixes      :: Set String
      , _grammar      :: Grammar
      }
  | ProposedParticle
      { _affixies     :: Set String
      }
  | Root
      { _affixes      :: Set String
      , _experimental :: Bool
      }
  | Compound
  | Loan
  | Name
  | Cluster
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Shape
makeLens ''Shape

data Definition
  = Definition
      { _definition :: String
      , _notes      :: Maybe String
      }
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Definition
makeLens ''Definition

data Word
  = Word
      { _word        :: String
      , _shape       :: Shape
      , _definitions :: Map Language [Revision Definition]
      }
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Word
makeLens ''Word

newtype ByWord = ByWord String deriving (Eq, Ord, Typeable)

instance Indexable Word
  where
    empty = ixSet [ixFun $ return . ByWord . _word]
