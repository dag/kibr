module Data.Kibr.Sitemap where

import Preamble

import Web.Routes.TH

data Sitemap
  = Home
  | Word Text
  | Stylesheet
  deriving (Eq, Ord, Read, Show, Data, Typeable)

derivePathInfo ''Sitemap
