module Preamble 
  ( (++)
  , filter
  , map
  , sum
  , Prelude.Bounded
  , Prelude.Enum
  , module Export
  ) where

import qualified Prelude

import Control.Applicative as Export
import Control.Category    as Export
import Control.Monad       as Export
import Data.Bool           as Export
import Data.Char           as Export
import Data.Either         as Export
import Data.Eq             as Export
import Data.Function       as Export hiding ((.), id)
import Data.Maybe          as Export
import Data.Ord            as Export
import Data.Tuple          as Export
import Text.Read           as Export (Read, read)
import Text.Show           as Export (Show, show)

(++) :: MonadPlus m => m a -> m a -> m a
(++) = mplus

filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter = mfilter

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

sum :: MonadPlus m => [m a] -> m a
sum = msum
