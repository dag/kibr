module Data.Kibr.Environment where

import Preamble

import Text.Blaze (Html)

import Data.Kibr.Language
import Data.Kibr.Message
import Data.Kibr.Sitemap
import Data.Kibr.State

data Environment
  = Environment
      { language :: Language
      , msg      :: Message -> Html
      , state    :: Acid
      , url      :: Sitemap -> Text
      }
