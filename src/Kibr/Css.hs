{-# LANGUAGE OverloadedStrings #-}

module Kibr.Css where

import Preamble

import Language.CSS

master :: CSS Rule
master = do

  rule "body" $ do
    margin "2em"

  rule "dt" $ do
    fontFamily "serif"
    fontSize "150%"
    fontWeight "500"
