module Preamble 
  ( (++)
  , filter
  , map
  , sum
  , Prelude.Bounded
  , Prelude.Enum
  , module X
  ) where

import qualified Prelude

import Control.Applicative as X ( Applicative, pure , (<*>), (*>), (<*), (<**>))
import Control.Category    as X
import Control.Monad       as X hiding ( fmap, return, fail, liftM, liftM2
                                       , liftM3, liftM4, liftM5, ap )
import Data.Bool           as X
import Data.Char           as X
import Data.Data           as X ( Data )
import Data.Either         as X
import Data.Eq             as X
import Data.Function       as X hiding ( (.), id )
import Data.Functor        as X ( Functor, (<$), (<$>) )
import Data.Int            as X
import Data.Map            as X ( Map )
import Data.Maybe          as X hiding ( fromJust )
import Data.Ord            as X
import Data.Set            as X ( Set )
import Data.Text           as X ( Text )
import Data.Tuple          as X
import Data.Typeable       as X ( Typeable )
import System.IO           as X ( IO )
import Text.Read           as X ( Read, read )
import Text.Show           as X ( Show, show )

(++) :: MonadPlus m => m a -> m a -> m a
(++) = mplus

filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter = mfilter

map :: Functor f => (a -> b) -> f a -> f b
map = (<$>)

sum :: MonadPlus m => [m a] -> m a
sum = msum
