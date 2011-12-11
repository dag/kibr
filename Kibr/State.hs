{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

module Kibr.State where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Kibr.Data

import qualified Data.Set as Set

writeState :: Dictionary -> Update Dictionary ()
writeState = put

readState :: Query Dictionary Dictionary
readState = ask

makeAcidic ''Dictionary ['writeState, 'readState]
