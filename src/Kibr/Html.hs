{-# LANGUAGE OverloadedStrings #-}

module Kibr.Html where

import Preamble

import Data.Lens
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Groom

import Language.Haskell.HsColour.ACSS (hscolour)

import Kibr.Data.State

import qualified Data.IxSet as Ix
import qualified Kibr.Data  as DB

linkCss :: AttributeValue -> Html
linkCss url = link ! href url ! rel "stylesheet" ! type_ "text/css"

master :: String -> Html -> Html
master styleUrl bodyContent
  = docTypeHtml $
      do head $
           do title "Lojban Dictionary"
              linkCss $ toValue styleUrl
              linkCss hscolourCss
         body bodyContent
    where hscolourCss = "http://code.haskell.org/~malcolm/hscolour/hscolour.css"

word :: DB.Word -> Html
word w
  = do dt . toHtml $ DB.word ^$ w
       dd . preEscapedString . hscolour False $ groom w

wordList :: State -> Html
wordList dict = dl . mapM_ word . Ix.toList $ words ^$ dict
