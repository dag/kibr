{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Kibr.Data.Word where

import Preamble

import Kibr.Data.Grammar
import Kibr.Data.Language
import Kibr.Data.Revision

import Data.IxSet as Ix
import Data.Lens.Template
import Data.Map
import Data.SafeCopy
import Data.Set

data Shape
  = Particle
      { _affixes      :: Set Text
      , _grammar      :: Grammar
      }
  | ProposedParticle
      { _affixies     :: Set Text
      }
  | Root
      { _affixes      :: Set Text
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
      { _definition :: Text
      , _notes      :: Maybe Text
      }
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Definition
makeLens ''Definition

data Word
  = Word
      { _word        :: Text
      , _shape       :: Shape
      , _definitions :: Map Language [Revision Definition]
      }
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Word
makeLens ''Word

newtype ByWord = ByWord Text deriving (Eq, Ord, Typeable)

data ByShape
  = IsParticle
  | IsProposedParticle
  | IsRoot
  | IsCompound
  | IsLoan
  | IsName
  | IsCluster
    deriving (Eq, Ord, Typeable)

byShape :: Shape -> ByShape
byShape Particle{..}         = IsParticle
byShape ProposedParticle{..} = IsProposedParticle
byShape Root{..}             = IsRoot
byShape Compound             = IsCompound
byShape Loan                 = IsLoan
byShape Name                 = IsName
byShape Cluster              = IsCluster

instance Indexable Word
  where
    empty = ixSet [ ixFun $ pure . ByWord . _word
                  , ixFun $ pure . byShape . _shape
                  ]
