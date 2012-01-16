{-# LANGUAGE OverloadedStrings #-}

module Kibr.Html where

import Preamble

import Data.Lens
import Happstack.Server (ServerPartT)
import System.IO (IO)
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Groom
import Web.Routes

import Language.Haskell.HsColour.ACSS (hscolour)

import Kibr.Data.Sitemap

import qualified Kibr.Data  as DB

type View = RouteT Sitemap (ServerPartT IO) Html

linkCss :: AttributeValue -> Html
linkCss url = link ! href url ! rel "stylesheet" ! type_ "text/css"

linkTo :: ToValue a => a -> Html -> Html
linkTo url = a ! href (toValue url)

master :: View -> View
master page =
  do
    contents <- page
    stylesheet <- showURL Stylesheet
    return . docTypeHtml $ do
      head $ do
        title "Lojban Dictionary"
        linkCss $ toValue stylesheet
        linkCss webfontsCss
      body contents
  where
    webfontsCss = "http://fonts.googleapis.com/css?family=Ubuntu+Mono:400,400italic,700,700italic|Ubuntu:400,400italic,700,700italic|Stoke"

wordList :: [DB.Word] -> View
wordList ws = do
  wd <- forM ws $ \w -> do
    let word = DB.word ^$ w
    wurl <- showURL $ Word word
    return $ do
      dt . linkTo wurl . toHtml $ word
      dd . preEscapedString . hscolour False $ groom w
  return . dl $ sequence_ wd
