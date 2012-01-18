{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Kibr.Data.Sitemap where

import Preamble

import Data.Data
import Data.Text
import Web.Routes.TH

data Sitemap
  = Home
  | Word Text
  | Stylesheet
    deriving (Eq, Ord, Read, Show, Data, Typeable)
derivePathInfo ''Sitemap
