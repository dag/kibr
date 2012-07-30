module Data.IxSet.Lens
    ( at
    )
  where

import Control.Lens  (Getter, to)
import Data.IxSet    (Indexable, IxSet, getOne, getEQ)
import Data.Typeable (Typeable)

at :: (Ord a, Typeable k, Typeable a, Indexable a)
   => k -> Getter (IxSet a) b (Maybe a) d
at k = to (getOne .getEQ k)
