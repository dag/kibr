module Data.Kibr.Environment where

import Preamble

import Control.Monad.Trans (MonadIO)
import Happstack.Server    (ServerPart, ServerPartT)

import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State

import qualified Control.Monad.Reader as R

data Environment
  = Environment
      { language :: Language
      , state    :: Acid
      , router   :: Sitemap -> [(Text, Maybe Text)] -> Text
      }

newtype Controller a
  = Controller (R.ReaderT Environment (ServerPartT IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadPlus
           , MonadIO
           , R.MonadReader Environment
           )

runController :: Controller a -> Environment -> ServerPart a
runController (Controller r) = R.runReaderT r

newtype Reader a
  = Reader (R.Reader Environment a)
  deriving ( Functor
           , Applicative
           , Monad
           , R.MonadReader Environment
           )

runReader :: Reader a -> Environment -> a
runReader (Reader r) = R.runReader r
