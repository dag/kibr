module Data.Kibr.Sitemap where

import Preamble

import Data.Kibr.Language

import Web.Routes.TH (derivePathInfo)

data Asset
  = Highlighter
  | Screen

derivePathInfo ''Asset

data Dictionary
  = Home
  | Word Text

derivePathInfo ''Dictionary

data Sitemap
  = Root
  | Asset Asset
  | Dictionary Language Dictionary

derivePathInfo ''Sitemap
