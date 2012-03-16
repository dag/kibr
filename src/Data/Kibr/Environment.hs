module Data.Kibr.Environment where

import Preamble

import Control.Monad.Trans (MonadIO)
import Happstack.Server    (ServerPart, ServerPartT, Response)
import Web.Routes          (MonadRoute(..))

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

newtype ControllerM a
  = ControllerM (R.ReaderT Environment (ServerPartT IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadPlus
           , MonadIO
           , R.MonadReader Environment
           )

type Controller = ControllerM Response

runControllerM :: ControllerM a -> Environment -> ServerPart a
runControllerM (ControllerM r) = R.runReaderT r

newtype Reader a
  = Reader (R.Reader Environment a)
  deriving ( Functor
           , Applicative
           , Monad
           , R.MonadReader Environment
           )

runReader :: Reader a -> Environment -> a
runReader (Reader r) = R.runReader r

instance MonadRoute ControllerM where
  type URL ControllerM = Sitemap
  askRouteFn = R.asks router

instance MonadRoute Reader where
  type URL Reader = Sitemap
  askRouteFn = R.asks router
