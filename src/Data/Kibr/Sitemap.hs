module Data.Kibr.Sitemap where

import Preamble

import Web.Routes.TH (derivePathInfo)

data Asset
  = Highlighter
  | Screen
  deriving (Eq, Ord, Read, Show, Data, Typeable)

derivePathInfo ''Asset

data Sitemap
  = Home
  | Word Text
  deriving (Eq, Ord, Read, Show, Data, Typeable)

derivePathInfo ''Sitemap
