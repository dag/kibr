module Preamble 
  ( ($)
  , (++)
  , (.)
  , Bool
  , Either(Left, Right)
  , Eq((==), (/=))
  , IO
  , String
  , filter
  , map
  , sum
  ) where

import Prelude
  ( ($)
  , (.)
  , Bool
  , Either(..)
  , Eq(..)
  , IO
  , String
  )

import Control.Monad

(++) :: MonadPlus m => m a -> m a -> m a
(++) = mplus

filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter = mfilter

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

sum :: MonadPlus m => [m a] -> m a
sum = msum
