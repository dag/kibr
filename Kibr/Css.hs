{-# LANGUAGE OverloadedStrings #-}

module Kibr.Css where

import Language.CSS

master :: CSS Rule
master = do

    rule "body" $ do
        margin "1em auto"
        maxWidth "600px"

    rule "dt" $ do
        fontFamily "serif"
        fontSize "150%"
        fontWeight "500"
