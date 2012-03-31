module Preamble
  ( enumerate
  , Prelude.Bounded
  , Prelude.Enum
  , module X
  ) where

import qualified Prelude

import Control.Applicative as X
import Control.Category    as X
import Control.Monad       as X hiding (fail)
import Data.Bool           as X
import Data.Char           as X
import Data.Data           as X (Data)
import Data.Default        as X
import Data.Either         as X
import Data.Eq             as X
import Data.Function       as X hiding ((.), id)
import Data.Int            as X
import Data.List           as X (map, (++))
import Data.Map            as X (Map)
import Data.Maybe          as X hiding (fromJust)
import Data.Ord            as X
import Data.Set            as X (Set)
import Data.Text           as X (Text)
import Data.Tuple          as X
import Data.Typeable       as X (Typeable)
import System.IO           as X (IO, FilePath)
import Text.Read           as X (Read, read)
import Text.Show           as X (Show, show)

#if DEVELOPMENT
import Debug.FileLocation  as X
#endif

enumerate :: (Prelude.Enum a, Prelude.Bounded a) => [a]
enumerate = [Prelude.minBound ..]
