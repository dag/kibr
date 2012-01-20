{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Kibr.Http where
 
import Preamble
import Prelude (error)

import Data.Acid.Advanced (query')
import Data.Lens
import Happstack.Server
import Language.CSS

import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State

import qualified Data.Text            as T
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
    startServer
  where
    setLogLevel =
      Log.updateGlobalLogger Log.rootLoggerName . Log.setLevel
    startServer =
      simpleHTTP config $ sum
        [ dir "resources" $ serveDirectory DisableBrowsing [] "resources"
        , nullDir >> seeOther ("/en/"::Text) (toResponse (""::Text))
        , locale "/en" English
        , locale "/jbo" Lojban
        ]
    site =
      R.setDefault Home . R.mkSitePI . R.runRouteT . route state
    locale code =
      R.implSite "" code . site

type Controller = R.RouteT Sitemap (ServerPartT IO) Response

route :: Acid -> Language -> Sitemap -> Controller
route st lang url =
  case url of
    Stylesheet -> stylesheet
    Home       -> home st lang
    Word w     -> word st lang w

stylesheet :: Controller
stylesheet = ok . toResponse $ Css.master

home :: Acid -> Language -> Controller
home st lang =
  do
    db <- query' st ReadState
    page <- Html.master lang . Html.wordList . Ix.toList $ db ^. words
    ok . toResponse $ page

word :: Acid -> Language -> T.Text -> Controller
word st lang w =
  do
    w' <- query' st . LookupWord $ w
    maybe mzero response w'
  where
    response w'' =
      do
        page <- Html.master lang . Html.wordList $ [w'']
        ok . toResponse $ page
