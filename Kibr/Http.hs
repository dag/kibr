module Kibr.Http where

import Happstack.Server
import Language.CSS
import Web.Routes
import Web.Routes.Happstack

import Kibr.Data.Sitemap
import Kibr.State (openState, loadState)

import qualified Data.Set  as Set
import qualified Kibr.Css  as Css
import qualified Kibr.Data as DB
import qualified Kibr.Html as Html

type Controller = RouteT Sitemap (ServerPartT IO) Response

route :: DB.Dictionary -> Sitemap -> Controller
route db url
  = case url
      of Home -> index db
         Word w -> word db w
         Stylesheet -> stylesheet

site :: DB.Dictionary -> Site Sitemap (ServerPartT IO Response)
site db = setDefault Home $ mkSitePI $ runRouteT $ route db

runHttp :: [String] -> IO ()
runHttp args
  = case parseConfig args
      of Left errors  -> mapM_ putStrLn errors
         Right config -> do state <- openState
                            db    <- loadState state
                            simpleHTTP config $ implSite "" "" $ site db

index :: DB.Dictionary -> Controller
index db
  = do style <- showURL Stylesheet
       ok . toResponse . Html.master style . Html.wordList $ db

word :: DB.Dictionary -> String -> Controller
word db w
  = do style <- showURL Stylesheet
       ok . toResponse . Html.master style . Html.word $ w'
    where w'  = Set.findMin . Set.filter p $ DB.words db
          p e = DB.word e == w

stylesheet :: Controller
stylesheet
  = ok . setHeader "Content-Type" "text/css"
       . toResponse . renderCSS . runCSS $ Css.master
