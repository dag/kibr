module Text.Kibr.Html where

import Preamble

import Control.Monad.Reader
import Data.Kibr.Environment
import Data.Kibr.Message
import Data.Kibr.Word
import Data.Lens
import Text.Blaze.Html5
import Text.Blaze.Html5.Extra
import Text.Blaze.Html5.Highlight
import Text.Groom

import qualified Data.Kibr.Sitemap as Url
import qualified Data.Text as T
import qualified Text.Highlighter.Lexers.Haskell as Haskell

type View = Reader Environment Html

master :: View -> View
master page =
  do
    msg <- asks msg
    page' <- page
    pure . docTypeHtml $ do
      head $ do
        title $ msg LojbanDictionary
        linkCss webfontsCss
        linkCss yuiCss
        linkCss highlighterCss
        linkCss masterCss
      body page'
  where
    webfontsCss    = "http://fonts.googleapis.com/css?family=Ubuntu+Mono:400,400italic,700,700italic|Ubuntu:400,400italic,700,700italic|Stoke"
    yuiCss         = "http://yui.yahooapis.com/combo?3.4.1/build/cssfonts/cssfonts-min.css&3.4.1/build/cssreset/cssreset-min.css&3.4.1/build/cssbase/cssbase-min.css"
    highlighterCss = "/resources/highlighter.css"
    masterCss      = "/resources/master.css"

wordList :: [Word] -> View
wordList ws =
  do
    url <- asks url
    pure . dl . forM_ ws $ \w -> do
      let w' = word ^$ w
      dt . linkTo (url $ Url.Word w') . toHtml $ w'
      dd . highlight Haskell.lexer False . T.pack . groom $ w
