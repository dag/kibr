module Kibr.Data.Revision where

import Preamble

import Data.Lens.Template
import Data.SafeCopy

data Revision a
  = Revision
      { _record  :: a
      , _comment :: Maybe Text
      }
  deriving (Eq, Show, Ord, Data, Typeable)

deriveSafeCopy 0 'base ''Revision

makeLens ''Revision
