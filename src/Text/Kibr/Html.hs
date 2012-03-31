module Text.Kibr.Html where

import Preamble

import Control.Monad.Reader       (asks)
import Data.Kibr.Message
import Data.Kibr.Sitemap
import Data.Kibr.Word             (Word, word)
import Data.Lens
import Text.Blaze.Html5
import Text.Blaze.Html5.Extra
import Text.Blaze.Html5.Highlight
import Text.Groom

import qualified Data.Kibr.Environment           as Env
import qualified Data.Text                       as T
import qualified Text.Highlighter.Lexers.Haskell as Haskell

type View = Env.Reader Html

makeTranslator :: Env.Reader (Message -> Html)
makeTranslator =
  do
    lang <- asks Env.language
    return $ toHtml . message lang

assetRouter :: Env.Reader (Asset -> Text)
assetRouter =
  do
    router <- asks Env.router
    return $ \x -> router (Asset x) []

dictionaryRouter :: Env.Reader (Dictionary -> Text)
dictionaryRouter =
  do
    lang   <- asks Env.language
    router <- asks Env.router
    return $ \x -> router (Dictionary lang x) []

master :: View -> View
master page =
  do
    msg     <- makeTranslator
    asset   <- assetRouter
    content <- page
    return . docTypeHtml $ do
      head $ do
        title $ msg LojbanDictionary
        mapM_ linkCss [webfonts, yui, asset Highlighter, asset Screen]
      body content
  where
    webfonts = T.concat [ "http://fonts.googleapis.com/css?family"
                        , "=Ubuntu+Mono:400,400italic,700,700italic"
                        , "|Ubuntu:400,400italic,700,700italic"
                        , "|Stoke"
                        ]
    yui      = T.concat [ "http://yui.yahooapis.com/combo"
                        , "?3.4.1/build/cssreset/cssreset-min.css"
                        , "&3.4.1/build/cssfonts/cssfonts-min.css"
                        , "&3.4.1/build/cssbase/cssbase-min.css"
                        ]

wordList :: [Word] -> View
wordList ws =
  do
    url <- dictionaryRouter
    return . dl . forM_ ws $ \w -> do
      let w' = word ^$ w
      dt . linkTo (url $ Word w') . toHtml $ w'
      dd . highlight Haskell.lexer False . T.pack . groom $ w
