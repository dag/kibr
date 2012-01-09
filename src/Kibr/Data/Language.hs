{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Kibr.Data.Language where

import Preamble

import Data.Data
import Data.SafeCopy

data Language
  = Lojban
  | English
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Language
