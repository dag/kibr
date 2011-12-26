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
    return . getOne $ ws @= ByWord w

reviseWord :: String
           -> Language
           -> Revision Definition
           -> Update Dictionary ()
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

makeAcidic ''Dictionary
  [ 'writeState
  , 'readState
  , 'lookupWord
  , 'reviseWord
  ]
