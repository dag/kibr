module Text.XHtmlCombinators.Extra where

import Preamble

import Text.XHtmlCombinators            as E
import Text.XHtmlCombinators.Attributes as A
import Text.XHtmlCombinators.Internal

linkCss :: Text -> XHtml HeadContent
linkCss url = link' [href url, rel "stylesheet", type_ "text/css"]

linkTo :: XHtml9 c => Text -> XHtml AContent -> XHtml c
linkTo url = a' [href url]
