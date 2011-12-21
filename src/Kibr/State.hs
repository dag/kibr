{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kibr.State where

import Prelude (,)
import Preamble

import Control.Monad.Reader (ask)
import Control.Monad.State  (put)
import Control.Monad.Trans  (MonadIO)
import Control.Monad.Trans  (liftIO)

import Kibr.Data

import qualified Data.Set  as Set

import qualified Data.Acid as Acid

type State = Acid.AcidState Dictionary

query :: (Acid.QueryEvent a, MonadIO m)
      => Acid.AcidState (Acid.EventState a)
      -> a
      -> m (Acid.EventResult a)
query st = liftIO . Acid.query st

writeState :: Dictionary -> Acid.Update Dictionary ()
writeState = put

readState :: Acid.Query Dictionary Dictionary
readState = ask

lookupWord :: String -> Acid.Query Dictionary (Maybe Word)
lookupWord w =
  do
    Dictionary words <- ask
    return . listToMaybe . Set.elems . Set.filter ((== w) . word) $ words

Acid.makeAcidic ''Dictionary ['writeState, 'readState, 'lookupWord]
