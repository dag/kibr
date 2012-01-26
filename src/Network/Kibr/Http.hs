{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Kibr.Http where
 
import Preamble
import Prelude (error)

import Control.Monad.Reader
import Data.Acid (QueryEvent, EventResult)
import Data.Acid.Advanced (query', MethodState)
import Data.Lens
import Data.List (last)
import Happstack.Server
import Happstack.Server.ETag
import Language.CSS
import Text.Blaze (toHtml)

import Data.Kibr.Environment
import Data.Kibr.Language
import Data.Kibr.Message
import Data.Kibr.Sitemap
import Data.Kibr.State

import qualified Data.IxSet           as Ix
import qualified Data.Text            as T
import qualified System.Log.Logger    as Log
import qualified Web.Routes           as R
import qualified Web.Routes.Happstack as R

import qualified Text.Kibr.Css  as Css
import qualified Text.Kibr.Html as Html

#ifndef DEVELOPMENT
import Happstack.Server.Compression
#endif

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
#if DEVELOPMENT
    setLogLevel Log.DEBUG
    simpleHTTP config $ sum
#else
    setLogLevel Log.WARNING
    simpleHTTP config $ compressedResponseFilter >> sum
#endif
      [ dir "resources" $ sum
          [ dir "master.css" stylesheet
          , serveDirectory DisableBrowsing [] "resources"
          ]
      , nullDir >> seeOther ("/English/"::Text) (toResponse (""::Text))
      , sum [ locale (T.pack ('/' : show lang)) lang | lang <- enumerate ]
      ]
  where
    setLogLevel      = Log.updateGlobalLogger Log.rootLoggerName . Log.setLevel
    locale code lang = do adler32ETagFilter
                          R.implSite "" code . R.setDefault Home . R.mkSitePI
                            $ route lang state

stylesheet :: ServerPart Response
stylesheet =
  do
    nullDir
    guardRq $ \rq -> last (rqUri rq) /= '/'
    method [GET, HEAD]
    adler32ETagFilter
    pure . toResponse $ Css.master

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
                , msg = toHtml . message lang
                , state = st
                , url = \s -> url s []
                }
    handler = case this of
                Home   -> home
                Word w -> word w

query :: ( MethodState event ~ State
         , QueryEvent event
         , MonadIO m
         , MonadReader Environment m
         )
      => event -> m (EventResult event)
query ev =
  do
    st <- asks state
    query' st ev

type Controller = ReaderT Environment (ServerPartT IO) Response

respond :: Html.View -> Controller
respond page =
  do
    env <- ask
    pure . toResponse . runReader (Html.master page) $ env

home :: Controller
home =
  do
    db <- query ReadState
    respond . Html.wordList . Ix.toList $ db ^. words

word :: Text -> Controller
word w =
  do
    w' <- query $ LookupWord w
    maybe mzero (respond . Html.wordList . pure) w'
