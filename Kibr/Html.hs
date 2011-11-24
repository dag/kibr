{-# LANGUAGE OverloadedStrings #-}

module Kibr.Html where


import Kibr.Data

import Text.Blaze
import Text.Groom

import qualified Data.Map                    as Map
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


word :: String -> Word -> Html
word name word = do
    Elem.dt $ toHtml name
    Elem.p $ Elem.dd $ toHtml $ groom word


wordList :: Dictionary -> Html
wordList dict =
    Elem.dl $ mapM_ (uncurry word) $ Map.toList dict
