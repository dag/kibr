module Data.Lens.Reader where

import Preamble

import Control.Monad.Reader
import Data.Lens

askL :: MonadReader r m => Lens r a -> m a
askL = asks . getL
