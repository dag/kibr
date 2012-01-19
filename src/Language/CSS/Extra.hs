module Language.CSS.Extra where

import Data.Either    (Either)
import Data.Text.Lazy (Text)
import Language.CSS

borderLeftColor :: Text -> CSS (Either Property Rule)
borderLeftColor = prop "border-left-color"
