module Text.Highlighter.Extra where

import Preamble

import Data.Text.Encoding               (encodeUtf8)
import Data.Text.Lazy                   (toStrict)
import Text.Blaze.Renderer.Text         (renderHtml)
import Text.Highlighter
import Text.Highlighter.Formatters.Html

highlight :: Lexer -> Bool -> Text -> Text
highlight lexer linenos text =
  case runLexer lexer bytes of
    Right tokens -> toStrict . renderHtml $ format linenos tokens
    Left _       -> text
  where
    bytes = encodeUtf8 text
