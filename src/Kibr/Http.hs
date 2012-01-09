module Kibr.Http where
 
import Preamble

import Data.Acid.Advanced (query')
import Data.Lens
import Language.CSS       (renderCSS, runCSS)

import Kibr.Data.Sitemap
import Kibr.Data.State

import qualified System.IO            as IO

import qualified Data.Acid            as Acid
import qualified Data.IxSet           as Ix
import qualified Happstack.Server     as H
import qualified System.Log.Logger    as Log
import qualified Web.Routes           as R
import qualified Web.Routes.Happstack as R

import qualified Kibr.Css             as Css
import qualified Kibr.Html            as Html

run :: [String] -> IO.IO ()
run args =
  case H.parseConfig args of
    Left errors  -> mapM_ IO.putStrLn errors
    Right config -> server config

server :: H.Conf -> IO.IO ()
server config =
  do
    setLogLevel Log.DEBUG
    state <- Acid.openLocalState $ State Ix.empty
    startServer state
    Acid.closeAcidState state
  where
    setLogLevel =
      Log.updateGlobalLogger Log.rootLoggerName . Log.setLevel
    startServer =
      H.simpleHTTP config . R.implSite "/" "" . site
    site =
      R.setDefault Home . R.mkSitePI . R.runRouteT . route

type Controller = R.RouteT Sitemap (H.ServerPartT IO.IO) H.Response

route :: Acid -> Sitemap -> Controller
route st url =
  case url of
    Home       -> home st
    Word w     -> word st w
    Stylesheet -> stylesheet

home :: Acid -> Controller
home st =
  do
    db <- query' st ReadState
    page <- Html.master . Html.wordList . Ix.toList $ db ^. words
    H.ok . H.toResponse $ page

word :: Acid -> String -> Controller
word st w =
  do
    w' <- query' st . LookupWord $ w
    maybe mzero response w'
  where
    response w'' =
      do
        page <- Html.master . Html.wordList $ [w'']
        H.ok . H.toResponse $ page

stylesheet :: Controller
stylesheet =
    H.ok . H.setHeader "Content-Type" "text/css"
         . H.toResponse
         . renderCSS
         . runCSS
         $ Css.master
