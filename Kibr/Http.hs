module Kibr.Http where

import Happstack.Server
import Language.CSS
import Web.Routes
import Web.Routes.Happstack

import Kibr.Css  as Css
import Kibr.Data
import Kibr.Data.Sitemap
import Kibr.Html as Html
import Kibr.State (openState, loadState)

route :: Dictionary -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route db url
  = case url
      of Home -> index db
         Stylesheet -> stylesheet

site :: Dictionary -> Site Sitemap (ServerPartT IO Response)
site db = setDefault Home $ mkSitePI $ runRouteT $ route db

runHttp :: [String] -> IO ()
runHttp args
  = case parseConfig args
      of Left errors  -> mapM_ putStrLn errors
         Right config -> do state <- openState
                            db    <- loadState state
                            simpleHTTP config $ implSite "" "" $ site db

index :: Dictionary -> RouteT Sitemap (ServerPartT IO) Response
index db
  = do style <- showURL Stylesheet
       ok . toResponse . Html.master style . wordList $ db

stylesheet :: RouteT Sitemap (ServerPartT IO) Response
stylesheet
  = ok . setHeader "Content-Type" "text/css"
       . toResponse . renderCSS . runCSS $ Css.master
