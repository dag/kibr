{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kibr.State where

import Preamble

import Control.Monad.Reader (ask)
import Control.Monad.State  (put)

import Data.Acid
import Data.IxSet as Ix
import Data.Lens

import Kibr.Data as DB

type State = AcidState Dictionary

writeState :: Dictionary -> Update Dictionary ()
writeState = put

readState :: Query Dictionary Dictionary
readState = ask

lookupWord :: String -> Query Dictionary (Maybe Word)
lookupWord w =
  do
    Dictionary ws <- ask
    return . listToMaybe . Ix.toList $ ws @= ByWord w

reviseWord :: String
           -> Language
           -> Revision Definition
           -> Update Dictionary ()
reviseWord w l r =
  do
    ws <- access words
    let [w'] = Ix.toList $ ws @= ByWord w
        ds   = mapLens l . definitions ^%= map (r:) $ w'
    words %= updateIx (ByWord w) ds
    return ()

makeAcidic ''Dictionary
  [ 'writeState
  , 'readState
  , 'lookupWord
  , 'reviseWord
  ]
