{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

-- | Provides the 'Packable' type class.
module Data.Packable
    ( Packable(..)
    , fromList
    , toList
    )
  where

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Foldable
import qualified Data.HashMap.Lazy
import qualified Data.HashSet
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.IxSet
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy

import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import Data.Word     (Word8)

-- | Alias for 'pack' to match the 'Show' instances of some types.
fromList :: Packable t a => [a] -> t
fromList = pack

-- | Alias for 'unpack' to complement 'fromList'.
toList :: Packable t a => t -> [a]
toList = unpack

-- | Types that can be unpacked and packed to and from lists.
class Packable t a | t -> a where
    pack   :: [a] -> t
    unpack :: t -> [a]

instance Packable [a] a where
    pack   = id
    unpack = id

instance Packable Data.ByteString.ByteString Word8 where
    pack   = Data.ByteString.pack
    unpack = Data.ByteString.unpack

instance Packable Data.ByteString.Lazy.ByteString Word8 where
    pack   = Data.ByteString.Lazy.pack
    unpack = Data.ByteString.Lazy.unpack

instance Packable Data.Text.Text Char where
    pack   = Data.Text.pack
    unpack = Data.Text.unpack

instance Packable Data.Text.Lazy.Text Char where
    pack   = Data.Text.Lazy.pack
    unpack = Data.Text.Lazy.unpack

instance Packable (Data.Sequence.Seq a) a where
    pack   = Data.Sequence.fromList
    unpack = Data.Foldable.toList


-- The following instances don't have the isomorphism properties
-- @pack . unpack = id@ and @unpack . pack = id@.

instance Packable (Maybe a) a where
    pack   = Data.Maybe.listToMaybe
    unpack = Data.Maybe.maybeToList

instance Ord a => Packable (Data.Set.Set a) a where
    pack   = Data.Set.fromList
    unpack = Data.Set.toList

instance Packable Data.IntSet.IntSet Int where
    pack   = Data.IntSet.fromList
    unpack = Data.IntSet.toList

instance Ord k => Packable (Data.Map.Map k a) (k,a) where
    pack   = Data.Map.fromList
    unpack = Data.Map.toList

instance Packable (Data.IntMap.IntMap a) (Data.IntMap.Key,a) where
    pack   = Data.IntMap.fromList
    unpack = Data.IntMap.toList

instance (Eq k, Hashable k) => Packable (Data.HashMap.Lazy.HashMap k a) (k,a) where
    pack   = Data.HashMap.Lazy.fromList
    unpack = Data.HashMap.Lazy.toList

instance (Eq a, Hashable a) => Packable (Data.HashSet.HashSet a) a where
    pack   = Data.HashSet.fromList
    unpack = Data.HashSet.toList

instance (Data.IxSet.Indexable a, Ord a, Typeable a) => Packable (Data.IxSet.IxSet a) a where
    pack   = Data.IxSet.fromList
    unpack = Data.IxSet.toList
