{-# LANGUAGE FlexibleContexts #-}

module Network.Kibr.Http where
 
import Preamble
import Prelude                (error, all)

import Control.Monad.Reader   (MonadReader, ask, asks)
import Control.Monad.Trans    (MonadIO)
import Data.Acid              (QueryEvent, EventResult)
import Data.Acid.Advanced     (query', MethodState)
import Data.FileEmbed         (embedFile)
import Data.Lens              ((^.))
import Data.List              (last)
import Happstack.Server
import Happstack.Server.ETag  (adler32ETagFilter)
import Language.CSS.Happstack ()
import Text.Blaze             (Html)
import Web.Routes             (mkSitePI, setDefault)
import Web.Routes.Happstack   (implSite)

import Data.Kibr.Environment
import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State

import qualified Data.IxSet        as Ix
import qualified Data.Text         as T
import qualified System.Log.Logger as Log

import qualified Text.Kibr.Css     as Css
import qualified Text.Kibr.Html    as Html

#if DEVELOPMENT
import Text.Blaze.Renderer.Pretty   (renderHtml)
#else
import Happstack.Server.Compression (compressedResponseFilter)
#endif

run :: [String] -> Acid -> IO ()
run args st =
  case parseConfig args of
    Left errors  -> error . join $ errors
    Right config -> server config st

server :: Conf -> Acid -> IO ()
server config st =
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
#ifndef DEVELOPMENT
    compressedResponseFilter >>
#endif
    sum
      [ implSite "" "/assets" . mkSitePI $ asset
      , sum [ locale (T.pack ('/' : show lang)) lang | lang <- enumerate ]
      , root
      , methodForbidden
      ]
  where
    methodForbidden =
      do
        method $ \m -> all (m /=) [GET, HEAD]
        resp 405 $ toResponse T.empty
    locale code lang =
      do
        adler32ETagFilter
        implSite "" code . setDefault Home . mkSitePI $ route lang st

root :: ServerPart Response
root =
  do
    nullDir
    method [GET, HEAD]
    seeOther defaultLocale emptyResponse
  where
    defaultLocale :: Text
    defaultLocale = "/English/"
    emptyResponse = toResponse T.empty

asset :: (Asset -> [(Text, Maybe Text)] -> Text)
      -> Asset -> ServerPart Response
asset _ Highlighter = highlighter
asset _ Screen      = stylesheet

filePart :: ServerPart ()
filePart =
  do
    nullDir
    guardRq $ \rq -> last (rqUri rq) /= '/'
    method [GET, HEAD]
    adler32ETagFilter

highlighter :: ServerPart Response
highlighter =
  do
    filePart
    setHeaderM "Content-Type" "text/css; charset=UTF-8"
    pure . toResponse $ $(embedFile "data/highlighter.css")

stylesheet :: ServerPart Response
stylesheet =
  do
    filePart
    pure . toResponse $ Css.master

route :: Language
      -> Acid
      -> (Sitemap -> [(Text, Maybe Text)] -> Text)
      -> Sitemap
      -> ServerPart Response
route lang st url' this =
    runController controller environ
  where
    environ    = Environment { language = lang
                             , state    = st
                             , url      = \s -> url' s []
                             }
    controller = case this of Home   -> home
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

respond :: View Html -> Controller Response
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
      . runView (Html.master page) $ env

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
