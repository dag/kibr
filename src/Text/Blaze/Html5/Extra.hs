module Text.Blaze.Html5.Extra where

import Preamble

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

linkCss :: Text -> Html
linkCss url = link ! href (toValue url) ! rel "stylesheet" ! type_ "text/css"

linkTo :: Text -> Html -> Html
linkTo url = a ! href (toValue url)
