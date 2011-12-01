{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

module Kibr.State where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Kibr.Data
import Kibr.Xml

import qualified Data.Set as Set

writeState :: Dictionary -> Update Dictionary ()
writeState = put

readState :: Query Dictionary Dictionary
readState = ask

makeAcidic ''Dictionary ['writeState, 'readState]

runImport :: [String] -> IO ()
runImport (file:args) =
  do
    db <- readDictionary English file
    state <- openState
    update state $ WriteState db
    closeAcidState state

openState :: IO (AcidState Dictionary)
openState = openLocalState $ Dictionary Set.empty

loadState :: (AcidState Dictionary) -> IO (EventResult ReadState)
loadState st = query st ReadState
