module Text.Blaze.Html5.Extra where

import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

linkCss :: AttributeValue -> Html
linkCss url = link ! href url ! rel "stylesheet" ! type_ "text/css"

linkTo :: ToValue a => a -> Html -> Html
linkTo url = a ! href (toValue url)
