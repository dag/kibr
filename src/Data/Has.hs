{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides the 'Has' type class.
module Data.Has
    ( Has(..)
    )
  where

-- | Types that can fetch a value by type without ambiguity.
class Has a m where
    fetch :: m a
