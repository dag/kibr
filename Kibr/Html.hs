{-# LANGUAGE OverloadedStrings #-}

module Kibr.Html where

import Prelude hiding (head)

import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Groom

import Language.Haskell.HsColour.ACSS (hscolour)

import qualified Data.Set  as Set
import qualified Kibr.Data as DB

linkCss :: AttributeValue -> Html
linkCss url = link ! href url ! rel "stylesheet" ! type_ "text/css"

master :: String -> Html -> Html
master style content
  = docTypeHtml $ do
    head $ do
      title "Lojban Dictionary"
      linkCss $ toValue style
      linkCss "http://code.haskell.org/~malcolm/hscolour/hscolour.css"
    body content

word :: DB.Word -> Html
word word
  = do dt $ toHtml $ DB.word word
       dd $ preEscapedString $ hscolour False $ groom word

wordList :: DB.Dictionary -> Html
wordList dict = dl $ mapM_ word $ Set.elems $ DB.words dict
