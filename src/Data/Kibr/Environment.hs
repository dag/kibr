module Data.Kibr.Environment where

import Preamble

import Control.Monad.Reader (ReaderT, Reader, MonadReader
                            ,runReaderT, runReader)
import Control.Monad.Trans  (MonadIO)
import Happstack.Server     (ServerPart, ServerPartT)

import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State

data Environment
  = Environment
      { language :: Language
      , state    :: Acid
      , url      :: Dictionary -> Text
      , asset    :: Asset -> Text
      }

newtype Controller a
  = Controller (ReaderT Environment (ServerPartT IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadPlus
           , MonadIO
           , MonadReader Environment
           )

runController :: Controller a -> Environment -> ServerPart a
runController (Controller r) = runReaderT r

newtype View a
  = View (Reader Environment a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Environment
           )

runView :: View a -> Environment -> a
runView (View r) = runReader r
