{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Kibr.Http where
 
import Preamble
import Prelude (error)

import Control.Monad.Reader
import Data.Acid.Advanced (query')
import Data.Lens
import Happstack.Server
import Language.CSS

import Data.Kibr.Environment
import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State
import Text.Blaze (Html)

import qualified Data.IxSet           as Ix
import qualified System.Log.Logger    as Log
import qualified Web.Routes           as R
import qualified Web.Routes.Happstack as R

import qualified Text.Kibr.Css        as Css
import qualified Text.Kibr.Html       as Html

instance ToMessage (CSS Rule) where
  toContentType _ = "text/css; charset=UTF-8"
  toMessage = toMessage . renderCSS . runCSS

run :: [String] -> Acid -> IO ()
run args state =
  case parseConfig args of
    Left errors  -> error . join $ errors
    Right config -> server config state

server :: Conf -> Acid -> IO ()
server config state =
  do
    setLogLevel Log.DEBUG
    simpleHTTP config $ sum
      [ dir "resources" $ sum
          [ dir "master.css" $ do
              nullDir
              ok . toResponse $ Css.master
          , serveDirectory DisableBrowsing [] "resources"
          ]
      , nullDir >> seeOther ("/en/"::Text) (toResponse (""::Text))
      , locale "/en" English
      , locale "/jbo" Lojban
      ]
  where
    setLogLevel      = Log.updateGlobalLogger Log.rootLoggerName . Log.setLevel
    locale code lang = R.implSite "" code . R.setDefault Home . R.mkSitePI
                       $ route lang state

route :: Language
      -> Acid
      -> (Sitemap -> [(Text, Maybe Text)] -> Text)
      -> Sitemap
      -> ServerPart Response
route lang st url this =
    runReaderT handler environ
  where
    environ = Environment
                { language = lang
                , state = st
                , url = \s -> url s []
                }
    handler = case this of
                Home   -> home
                Word w -> word w

type Controller = ReaderT Environment (ServerPartT IO) Response

respond :: (Environment -> Html) -> Controller
respond page =
  do
    env <- ask
    pure . toResponse . Html.master env $ page env

home :: Controller
home =
  do
    st <- asks state
    db <- query' st ReadState
    respond . Html.wordList . Ix.toList $ db ^. words

word :: Text -> Controller
word w =
  do
    st <- asks state
    w' <- query' st . LookupWord $ w
    maybe mzero (respond . Html.wordList . pure) w'
