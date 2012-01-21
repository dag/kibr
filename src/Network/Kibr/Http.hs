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

import Data.Kibr.Environment
import Data.Kibr.Language
import Data.Kibr.Sitemap
import Data.Kibr.State

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
    site             = R.setDefault Home . R.mkSitePI . R.runRouteT . route
    locale code lang = let env = Environment { language = lang, state = state }
                       in R.implSite "" code . site $ env

type Controller = R.RouteT Sitemap (ServerPartT IO) Response

respond :: Environment -> Html.View -> Controller
respond env@Environment{..} page =
  do
    html <- Html.master env page
    ok . toResponse $ html

route :: Environment -> Sitemap -> Controller
route env url =
  case url of
    Home   -> home env
    Word w -> word env w

home :: Environment -> Controller
home env@Environment{..} =
  do
    db <- query' state ReadState
    respond env . Html.wordList . Ix.toList $ db ^. words

word :: Environment -> Text -> Controller
word env@Environment{..} w =
  do
    w' <- query' state . LookupWord $ w
    maybe mzero (respond env . Html.wordList . pure) w'
