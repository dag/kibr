module Kibr.Data where


data Word
    = Word
        { name       :: String
        , shape      :: Shape
        , definition :: String
        , notes      :: Maybe String
        }
      deriving (Eq, Show)


data Shape
    = Particle
        { affixes      :: [String]
        , experimental :: Bool
        , grammar      :: String
        }
    | Root
        { affixes      :: [String]
        , experimental :: Bool
        }
    | Compound
    | Loan
    | Name
    | Cluster
      deriving (Eq, Show)
