{-# LANGUAGE FlexibleContexts #-}

module Network.Kibr.Http where

import Preamble
import Prelude                  (notElem)

import Control.Monad.Reader     (MonadReader, ask, asks)
import Control.Monad.Trans      (MonadIO)
import Data.Acid                (QueryEvent, EventResult)
import Data.Acid.Advanced       (query', MethodState)
import Data.FileEmbed.Happstack (embedFileAsResponse)
import Data.Lens                ((^.))
import Data.List                (last)
import Happstack.Server
import Happstack.Server.ETag    (adler32ETagFilter)
import Language.CSS.Happstack   ()
import Web.Routes               (mkSitePI, setDefault)
import Web.Routes.Happstack     (implSite)

import Data.Kibr.Environment
import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State

import qualified Data.IxSet        as Ix
import qualified System.Log.Logger as Log

import qualified Text.Kibr.Css     as Css
import qualified Text.Kibr.Html    as Html

#if DEVELOPMENT
import Text.Blaze.Renderer.Pretty   (renderHtml)
#else
import Happstack.Server.Compression (compressedResponseFilter)
#endif

run :: Conf -> Acid -> IO ()
run config st =
  do
    setLogLevel
#if DEVELOPMENT
      Log.DEBUG
#else
      Log.WARNING
#endif
    simpleHTTP config $ master st
  where
    setLogLevel = Log.updateGlobalLogger Log.rootLoggerName . Log.setLevel

master :: Acid -> ServerPart Response
master st =
    sum [ implSite "" "" . setDefault Root . mkSitePI $ sitemap st
        , methodForbidden
        ]
  where
    methodForbidden =
      do
        method (`notElem` [GET, HEAD])
        resp 405 $ toResponse ()

sitemap :: Acid
        -> (Sitemap -> [(Text, Maybe Text)] -> Text)
        -> Sitemap
        -> ServerPart Response
sitemap st url' this =
  do
#ifndef DEVELOPMENT
    compressedResponseFilter
#endif
    adler32ETagFilter
    case this of
      Root                 -> root $ url' (Dictionary English Home) []
      Asset Highlighter    -> highlighter
      Asset Screen         -> stylesheet
      Dictionary lang page -> dictionary lang st url' page

root :: Text -> ServerPart Response
root home' =
  do
    nullDir
    method [GET, HEAD]
    seeOther home' $ toResponse ()

filePart :: ServerPart ()
filePart =
  do
    nullDir
    guardRq $ \rq -> last (rqUri rq) /= '/'
    method [GET, HEAD]

highlighter :: ServerPart Response
highlighter =
  do
    filePart
    pure $(embedFileAsResponse "data/highlighter.css")

stylesheet :: ServerPart Response
stylesheet =
  do
    filePart
    pure . toResponse $ Css.master

dictionary :: Language
           -> Acid
           -> (Sitemap -> [(Text, Maybe Text)] -> Text)
           -> Dictionary
           -> ServerPart Response
dictionary lang st url' page =
    runController controller environ
  where
    environ    = Environment { language = lang
                             , state    = st
                             , url      = \s -> url' (Dictionary lang s) []
                             , asset    = \s -> url' (Asset s) []
                             }
    controller = case page of Home   -> home
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

respond :: Html.View -> Controller Response
respond page =
  do
    env <- ask
    pure
#if DEVELOPMENT
      . setHeader "Content-Type" "text/html; charset=UTF-8"
      . toResponse
      . renderHtml
#else
      . toResponse
#endif
      . runReader (Html.master page) $ env

home :: Controller Response
home =
  do
    db <- query ReadState
    respond . Html.wordList . Ix.toList $ db ^. words

word :: Text -> Controller Response
word w =
  do
    w' <- query $ LookupWord w
    maybe mzero (respond . Html.wordList . pure) w'
