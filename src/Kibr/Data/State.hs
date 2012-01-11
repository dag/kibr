{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kibr.Data.State where

import Preamble

import Control.Monad.Reader (ask, asks)
import Control.Monad.State  (put)

import Data.Acid
import Data.Data
import Data.IxSet as Ix
import Data.Lens
import Data.Lens.Template
import Data.SafeCopy

import Kibr.Data as DB

ixLens :: (Indexable a, Typeable a, Typeable k, Ord a)
       => k -> Lens (IxSet a) (Maybe a)
ixLens k = lens get set
  where
    get          = getOne . getEQ k
    set (Just v) = updateIx k v
    set Nothing  = deleteIx k

data State
  = State { _words :: IxSet Word }
    deriving (Eq, Show, Data, Typeable)
deriveSafeCopy 0 'base ''State
makeLens ''State

type Acid = AcidState State

writeState :: State -> Update State ()
writeState = put

readState :: Query State State
readState = ask

lookupWord :: String -> Query State (Maybe Word)
lookupWord w = asks . getL $ ixLens (ByWord w) . words

reviseWord :: String -> Language -> Revision Definition -> Update State ()
reviseWord w l r =
  do
    word . words %= map (lang . definitions ^%= map (r:))
    return ()
  where
    word = ixLens $ ByWord w
    lang = mapLens l

makeAcidic ''State
  [ 'writeState
  , 'readState
  , 'lookupWord
  , 'reviseWord
  ]
