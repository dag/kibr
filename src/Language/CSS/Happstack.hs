{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.CSS.Happstack where

import Preamble

import Happstack.Server
import Language.CSS

instance ToMessage (CSS Rule) where
  toContentType _ = "text/css; charset=UTF-8"
  toMessage = toMessage . renderCSS . runCSS
