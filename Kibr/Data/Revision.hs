{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Kibr.Data.Revision where

import Data.Data
import Data.SafeCopy

data Revision a
  = Revision
      { record  :: a
      , comment :: Maybe String
      }
    deriving (Eq, Show, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Revision
