module Control.Lens.TH.Extra
    ( prime
    , makeLenses
    )
  where

import Control.Lens.TH     (makeLensesBy)
import Language.Haskell.TH (Name, DecsQ)

prime :: String -> Maybe String
prime s | last s == '\'' = Just $ take (length s - 1) s
        | otherwise      = Nothing

makeLenses :: Name -> DecsQ
makeLenses = makeLensesBy prime
