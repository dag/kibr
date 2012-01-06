{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kibr.Data.State where

import Preamble

import Control.Monad.Reader (ask)
import Control.Monad.State  (put)

import Data.Acid
import Data.Data
import Data.IxSet as Ix
import Data.Lens
import Data.Lens.Template
import Data.SafeCopy

import Kibr.Data as DB

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
lookupWord w =
  do
    State ws <- ask
    return . getOne $ ws @= ByWord w

reviseWord :: String
           -> Language
           -> Revision Definition
           -> Update State ()
reviseWord w l r =
  do
    ws <- access words
    case getOne $ ws @= ByWord w of
      Just w' ->
        do
          words %= updateIx (ByWord w)
                     (mapLens l . definitions ^%= map (r:) $ w')
          return ()
      Nothing -> return ()

makeAcidic ''State
  [ 'writeState
  , 'readState
  , 'lookupWord
  , 'reviseWord
  ]
