module Data.Kibr.Word where

import Preamble

import Data.Kibr.Grammar
import Data.Kibr.Language
import Data.Kibr.Revision

import Data.ConstructorTag
import Data.HiggsSet as Higgs
import Data.Lens.Template
import Data.SafeCopy

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

makeConstructorTags ''Shape [''Eq, ''Ord, ''Typeable]

instance Indexable Word where
  empty = ixSet [ ixFun $ pure . ByWord . _word
                , ixFun $ pure . byShape . _shape
                ]
