module Data.Kibr.Environment where

import Preamble

import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State

data Environment
  = Environment
      { language :: Language
      , state    :: Acid
      , url      :: Sitemap -> Text
      }
