module Data.Kibr.Sitemap where

import Preamble

import Web.Routes.TH (derivePathInfo)

data Asset
  = Highlighter
  | Screen

derivePathInfo ''Asset

data Sitemap
  = Home
  | Word Text

derivePathInfo ''Sitemap
