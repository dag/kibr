module Data.Kibr.Environment where

import Preamble

import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.Trans  (MonadIO)

import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State

data Environment
  = Environment
      { language :: Language
      , state    :: Acid
      , url      :: Sitemap -> Text
      }

newtype Environmental m a = Environmental (ReaderT Environment m a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadPlus
           , MonadIO
           , MonadReader Environment
           )

runEnvironment :: Environment -> Environmental m a -> m a
runEnvironment e (Environmental r) = runReaderT r e
