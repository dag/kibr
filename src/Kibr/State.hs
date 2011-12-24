{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kibr.State where

import Preamble

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Map             ((!))

import Data.Acid
import Data.Lens

import Kibr.Data as DB

import qualified Data.Map  as Map
import qualified Data.Set  as Set

type State = AcidState Dictionary

writeState :: Dictionary -> Update Dictionary ()
writeState = put

readState :: Query Dictionary Dictionary
readState = ask

lookupWord :: String -> Query Dictionary (Maybe Word)
lookupWord w =
  do
    Dictionary words <- ask
    return . listToMaybe . Set.elems . Set.filter ((== w) . getL word) $ words

reviseWord :: String
           -> Language
           -> Revision Definition
           -> Update Dictionary ()
reviseWord w l r =
  do
    db <- get
    let ws   = db ^. words
        [w'] = Set.elems . Set.filter ((== w) . getL word) $ ws
        ds   = mapLens l . definitions ^%= map (r:) $ w'
    words %= Set.delete w' . Set.insert ds
    return ()

makeAcidic ''Dictionary
  [ 'writeState
  , 'readState
  , 'lookupWord
  , 'reviseWord
  ]
