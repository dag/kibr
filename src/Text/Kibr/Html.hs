module Text.Kibr.Html where

import Preamble

import Data.Kibr.Word
import Data.Lens
import Happstack.Server (ServerPartT)
import Language.Haskell.HsColour.ACSS (hscolour)
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html5.Extra
import Text.Groom
import Web.Routes

import qualified Data.Kibr.Sitemap as Url

type View = RouteT Url.Sitemap (ServerPartT IO) Html

master :: View -> View
master page =
  do
    contents <- page
    stylesheet <- showURL Url.Stylesheet
    pure . docTypeHtml $ do
      head $ do
        title "Lojban Dictionary"
        linkCss webfontsCss
        linkCss yuiCss
        linkCss $ toValue stylesheet
      body contents
  where
    webfontsCss = "http://fonts.googleapis.com/css?family=Ubuntu+Mono:400,400italic,700,700italic|Ubuntu:400,400italic,700,700italic|Stoke"
    yuiCss = "http://yui.yahooapis.com/combo?3.4.1/build/cssfonts/cssfonts-min.css&3.4.1/build/cssreset/cssreset-min.css&3.4.1/build/cssbase/cssbase-min.css"

wordList :: [Word] -> View
wordList ws = do
  wd <- forM ws $ \w -> do
    let w' = word ^$ w
    wurl <- showURL . Url.Word $ w'
    pure $ do
      dt . linkTo wurl . toHtml $ w'
      dd . preEscapedString . hscolour False $ groom w
  pure . dl $ sequence_ wd
