{-# LANGUAGE OverloadedStrings #-}

module Kibr.Html where

import Text.Blaze
import Text.Groom

import qualified Data.Set                    as Set
import qualified Kibr.Data                   as DB
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

word :: DB.Word -> Html
word word = do
    Elem.dt $ toHtml $ DB.word word
    Elem.dd $ Elem.pre $ toHtml $ groom word

wordList :: DB.Dictionary -> Html
wordList dict =
    Elem.dl $ mapM_ word $ Set.elems $ DB.words dict
