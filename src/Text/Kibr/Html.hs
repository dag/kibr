module Text.Kibr.Html where

import Preamble

import Data.Kibr.Environment
import Data.Kibr.Message
import Data.Kibr.Word
import Data.Lens
import Happstack.Server (ServerPartT)
import Text.Blaze.Html5
import Text.Blaze.Html5.Extra
import Text.Blaze.Html5.Highlight
import Text.Groom
import Web.Routes

import qualified Data.Kibr.Sitemap as Url
import qualified Data.Text as T
import qualified Text.Highlighter.Lexers.Haskell as Haskell

type View = RouteT Url.Sitemap (ServerPartT IO) Html

master :: Environment -> View -> View
master Environment{..} page =
  do
    contents <- page
    pure . docTypeHtml $ do
      head $ do
        title $ translate LojbanDictionary
        linkCss webfontsCss
        linkCss yuiCss
        linkCss highlighterCss
        linkCss masterCss
      body contents
  where
    translate      = toHtml . msg language
    webfontsCss    = "http://fonts.googleapis.com/css?family=Ubuntu+Mono:400,400italic,700,700italic|Ubuntu:400,400italic,700,700italic|Stoke"
    yuiCss         = "http://yui.yahooapis.com/combo?3.4.1/build/cssfonts/cssfonts-min.css&3.4.1/build/cssreset/cssreset-min.css&3.4.1/build/cssbase/cssbase-min.css"
    highlighterCss = "/resources/highlighter.css"
    masterCss      = "/resources/master.css"

wordList :: [Word] -> View
wordList ws = do
  wd <- forM ws $ \w -> do
    let w' = word ^$ w
    wurl <- showURL . Url.Word $ w'
    pure $ do
      dt . linkTo wurl . toHtml $ w'
      dd . highlight Haskell.lexer False . T.pack . groom $ w
  pure . dl $ sequence_ wd
