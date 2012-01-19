module Text.Blaze.Html5.Highlight where

import Preamble

import Data.Text.Encoding (encodeUtf8)
import Text.Blaze.Html5
import Text.Highlighter
import Text.Highlighter.Formatters.Html

highlight :: Lexer -> Bool -> Text -> Html
highlight lexer linenos text =
  case runLexer lexer bytes of
    Right tokens -> format linenos tokens
    Left _       -> toHtml text
  where
    bytes = encodeUtf8 text
