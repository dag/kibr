{-# LANGUAGE OverloadedStrings #-}

module Kibr.Css where

import Language.CSS


master :: CSS Rule
master = do

    rule "body" $ do
        margin "1em auto"
        maxWidth "600px"

    rule "dd" $ do
        fontFamily "monospace"
