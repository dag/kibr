{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kibr.State where

import Preamble

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)

import Data.Acid

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
    return . listToMaybe . Set.elems . Set.filter ((== w) . word) $ words

reviseWord :: String
           -> Language
           -> Revision Definition
           -> Update Dictionary ()
reviseWord w l r =
  do
    d@Dictionary{..} <- get
    let [w'@Word{..}] = Set.elems . Set.filter ((== w) . DB.word) $ words
    put d { words = Set.delete w' $ Set.insert w'
                      { definitions =
                          Map.insert l (r : definitions Map.! l) definitions
                      } words
          }

makeAcidic ''Dictionary
  [ 'writeState
  , 'readState
  , 'lookupWord
  , 'reviseWord
  ]
