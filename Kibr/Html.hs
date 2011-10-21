{-# LANGUAGE OverloadedStrings #-}

module Kibr.Html where

import Kibr.Data

import Text.Blaze

import qualified Text.Blaze.Html5            as Elem
import qualified Text.Blaze.Html5.Attributes as Attr


linkCss :: AttributeValue -> Html
linkCss url =
    Elem.link
        ! Attr.href url
        ! Attr.rel "stylesheet"
        ! Attr.type_ "text/css"


master :: Html -> Html
master content =
    Elem.docTypeHtml $ do
        Elem.head $ do
            Elem.title "Lojban Dictionary"
            linkCss "/master.css"
        Elem.body content


word :: Word -> Html
word w = do
    Elem.dt $ toHtml $ name w
    Elem.p $ Elem.dd $ toHtml $ definition w


wordList :: [Word] -> Html
wordList ws =
    Elem.dl $ mapM_ word $ filter isRoot ws
  where
    isRoot w =
      case shape w of
           Root _ _ -> True
           _        -> False
