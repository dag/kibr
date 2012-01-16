{-# LANGUAGE OverloadedStrings #-}

module Kibr.Css where

import Preamble

import Language.CSS

master :: CSS Rule
master = do

  rule "body" $ do
    fontFamily "'Ubuntu', sans-serif"
    lineHeight "150%"
    margin "auto"
    maxWidth "800px"
    padding "2em"

  rule "a" $ do
    color "#1160AC"
    textDecoration "none"

  rule "dt" $ do
    fontFamily "'Stoke', serif"
    fontSize "150%"

  rule "pre, code, tt" $ do
    fontFamily "'Ubuntu Mono', monospace"

  rule "pre" $ do
    background "#fafafa"
    borderLeft "5px solid #eee"
    fontSize "11pt"
    overflow "hidden"
    padding ".5em"

  rule ".hs-str" $ do
    background "#fff0f0"
    color "#dd2200"

  rule ".hs-conid" $ do
    color "#888"
    fontWeight "bold"

  rule ".hs-keyglyph" $ do
    color "#080"
