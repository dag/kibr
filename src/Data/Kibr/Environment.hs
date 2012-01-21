module Data.Kibr.Environment where

import Data.Kibr.Language
import Data.Kibr.State

data Environment
  = Environment
      { language :: Language
      , state    :: Acid
      }
