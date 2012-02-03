module Data.Kibr.Word where

import Preamble
import Prelude (undefined, minBound, maxBound, fromEnum, toEnum)

import Data.Kibr.Grammar
import Data.Kibr.Language
import Data.Kibr.Revision

import Data.ConstructorTag
import Data.HiggsSet as Higgs
import Data.Lens.Template
import Data.SafeCopy
import Data.TrieMap.Representation

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

makeConstructorTags ''Shape [''Eq, ''Ord, ''Typeable]

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

data WordIndex
  = ByWord Text
  | ByShape ByShape
  deriving (Eq, Ord)

genOrdRepr ''WordIndex

instance Bounded WordIndex where
  minBound = ByWord undefined
  maxBound = ByShape undefined

instance Enum WordIndex where
  fromEnum i =
    case i of
      ByWord _  -> 0
      ByShape _ -> 1
  toEnum 0 = ByWord undefined
  toEnum 1 = ByShape undefined
  toEnum _ = undefined

instance Index WordIndex

instance Indexable Word where
  type IndexOf Word = WordIndex
  project i x =
    case i of
      ByWord _  -> [ByWord . _word $ x]
      ByShape _ -> [ByShape . byShape . _shape $ x]
