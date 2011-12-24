{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Kibr.Data.Sitemap where

import Data.Data
import Web.Routes.TH

data Sitemap
  = Home
  | Word String
  | Stylesheet
    deriving (Eq, Ord, Read, Show, Data, Typeable)
derivePathInfo ''Sitemap