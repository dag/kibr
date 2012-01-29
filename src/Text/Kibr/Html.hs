module Text.Kibr.Html where

import Preamble

import Control.Monad.Identity     (Identity)
import Control.Monad.Reader       (asks)
import Data.Kibr.Message
import Data.Kibr.Word
import Data.Lens
import Text.Blaze.Html5
import Text.Blaze.Html5.Extra
import Text.Blaze.Html5.Highlight
import Text.Groom

import qualified Data.Kibr.Sitemap               as Url
import qualified Data.Kibr.Environment           as Env
import qualified Data.Text                       as T
import qualified Text.Highlighter.Lexers.Haskell as Haskell

type View = Env.Environmental Identity Html

makeTranslator :: Env.Environmental Identity (Message -> Html)
makeTranslator =
  do
    lang <- asks Env.language
    pure $ toHtml . message lang

master :: View -> View
master page =
  do
    msg <- makeTranslator
    page' <- page
    pure . docTypeHtml $ do
      head $ do
        title $ msg LojbanDictionary
        mapM_ linkCss
          [ webfonts
          , yui
          , highlighter
          , masterCss
          ]
      body page'
  where
    webfonts    = T.concat [ "http://fonts.googleapis.com/css?family"
                           , "=Ubuntu+Mono:400,400italic,700,700italic"
                           , "|Ubuntu:400,400italic,700,700italic"
                           , "|Stoke"
                           ]
    yui         = T.concat [ "http://yui.yahooapis.com/combo"
                           , "?3.4.1/build/cssreset/cssreset-min.css"
                           , "&3.4.1/build/cssfonts/cssfonts-min.css"
                           , "&3.4.1/build/cssbase/cssbase-min.css"
                           ]
    highlighter = "/resources/highlighter.css"
    masterCss   = "/resources/master.css"

wordList :: [Word] -> View
wordList ws =
  do
    url <- asks Env.url
    pure . dl . forM_ ws $ \w -> do
      let w' = word ^$ w
      dt . linkTo (url $ Url.Word w') . toHtml $ w'
      dd . highlight Haskell.lexer False . T.pack . groom $ w
