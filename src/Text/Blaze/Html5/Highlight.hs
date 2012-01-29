module Text.Blaze.Html5.Highlight where

import Preamble

import Data.Text.Encoding (encodeUtf8)
import Text.Blaze.Html5 hiding (text)
import Text.Highlighter
import Text.Highlighter.Formatters.Html

#if DEVELOPMENT
import Text.Blaze.Renderer.String (renderHtml)
import qualified Data.Text as T
#endif

highlight :: Lexer -> Bool -> Text -> Html
highlight lexer linenos text =
  case runLexer lexer bytes of
    Right tokens -> preHtml $ format linenos tokens
    Left _       -> toHtml text
  where
    bytes = encodeUtf8 text
#if DEVELOPMENT
    preHtml = preEscapedText . T.pack . renderHtml
#else
    preHtml = id
#endif
