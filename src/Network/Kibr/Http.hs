{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Kibr.Http where
 
import Preamble

import Data.Acid.Advanced (query')
import Data.Lens
import Language.CSS

import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State

import qualified System.IO            as IO

import qualified Data.Text            as T
import qualified Data.IxSet           as Ix
import qualified Happstack.Server     as H
import qualified System.Log.Logger    as Log
import qualified Web.Routes           as R
import qualified Web.Routes.Happstack as R

import qualified Text.Kibr.Css        as Css
import qualified Text.Kibr.Html       as Html

instance H.ToMessage (CSS Rule) where
  toContentType _ = "text/css; charset=UTF-8"
  toMessage = H.toMessage . renderCSS . runCSS

run :: [String] -> Acid -> IO.IO ()
run args state =
  case H.parseConfig args of
    Left errors  -> mapM_ IO.putStrLn errors
    Right config -> server config state

server :: H.Conf -> Acid -> IO.IO ()
server config state =
  do
    setLogLevel Log.DEBUG
    startServer
  where
    setLogLevel =
      Log.updateGlobalLogger Log.rootLoggerName . Log.setLevel
    startServer =
      H.simpleHTTP config $ sum
        [ H.dir "resources" $ H.serveDirectory H.DisableBrowsing [] "resources"
        , H.nullDir >> H.seeOther ("/en/"::Text) (H.toResponse (""::Text))
        , locale "/en" English
        , locale "/jbo" Lojban
        ]
    site =
      R.setDefault Home . R.mkSitePI . R.runRouteT . route state
    locale code =
      R.implSite "" code . site

type Controller = R.RouteT Sitemap (H.ServerPartT IO.IO) H.Response

route :: Acid -> Language -> Sitemap -> Controller
route st lang url =
  case url of
    Home       -> home st lang
    Word w     -> word st lang w
    Stylesheet -> stylesheet

home :: Acid -> Language -> Controller
home st lang =
  do
    db <- query' st ReadState
    page <- Html.master lang . Html.wordList . Ix.toList $ db ^. words
    H.ok . H.toResponse $ page

word :: Acid -> Language -> T.Text -> Controller
word st lang w =
  do
    w' <- query' st . LookupWord $ w
    maybe mzero response w'
  where
    response w'' =
      do
        page <- Html.master lang . Html.wordList $ [w'']
        H.ok . H.toResponse $ page

stylesheet :: Controller
stylesheet = H.ok . H.toResponse $ Css.master
