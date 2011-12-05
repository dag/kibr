{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Kibr.Data.Sitemap where

import Data.Data
import Web.Routes.TH

data Sitemap
  = Home
  | Stylesheet
    deriving (Eq, Ord, Read, Show, Data, Typeable)
derivePathInfo ''Sitemap
