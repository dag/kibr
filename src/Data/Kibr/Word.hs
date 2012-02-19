module Data.Kibr.Word where

import Preamble

import Data.Kibr.Grammar
import Data.Kibr.Language
import Data.Kibr.Revision

import Data.ConstructorTag
import Data.IxSet as Ix
import Data.Lens.Template
import Data.SafeCopy

import qualified Data.Set as Set

data Shape
  = Particle
      { _affixes      :: Set Text
      , _grammar      :: Grammar
      }
  | ProposedParticle
      { _affixes      :: Set Text
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

newtype ByAffix = ByAffix Text deriving (Eq, Ord, Typeable)

makeConstructorTags ''Shape [''Eq, ''Ord, ''Typeable]

instance Indexable Word where
  empty =
      ixSet [ ixFun $ pure . ByWord . _word
            , ixFun $ map ByAffix . getAffixes . _shape
            , ixFun $ pure . byShape . _shape
            ]
    where
      getAffixes Particle{..}         = Set.toList _affixes
      getAffixes ProposedParticle{..} = Set.toList _affixes
      getAffixes Root{..}             = Set.toList _affixes
      getAffixes _                    = []
