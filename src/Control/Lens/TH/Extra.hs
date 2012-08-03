module Control.Lens.TH.Extra
    ( prime
    , makeLenses
    )
  where

import Control.Lens        ((<~))
import Control.Lens.TH     (makeLensesWith, lensField, lensIso, defaultRules)
import Language.Haskell.TH (Name, DecsQ)

prime :: String -> Maybe String
prime s | last s == '\'' = Just $ init s
        | otherwise      = Nothing

makeLenses :: Name -> DecsQ
makeLenses =
    makeLensesWith
      $ lensField <~ prime
      $ lensIso   <~ const Nothing
      $ defaultRules
