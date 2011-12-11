module Kibr.Http where

import Control.Monad.Trans
import Data.Acid
import Happstack.Server
import Kibr.Data.Sitemap
import Kibr.State
import Language.CSS
import System.Log.Logger
import Web.Routes
import Web.Routes.Happstack

import qualified Data.Set  as Set
import qualified Kibr.Css  as Css
import qualified Kibr.Data as DB
import qualified Kibr.Html as Html

runHttp :: [String] -> IO ()
runHttp args
  = case parseConfig args
      of Left errors  -> mapM_ putStrLn errors
         Right config -> server config

server :: Conf -> IO ()
server config
  = do updateGlobalLogger rootLoggerName $ setLevel DEBUG
       state <- openState
       simpleHTTP config . implSite "/" ""
                         . setDefault Home
                         . mkSitePI
                         . runRouteT
                         . route
                         $ state
       closeAcidState state

type State      = AcidState DB.Dictionary
type Controller = RouteT Sitemap (ServerPartT IO) Response

route :: State -> Sitemap -> Controller
route st url
  = case url
      of Home       -> home st
         Word w     -> word st w
         Stylesheet -> stylesheet

home :: State -> Controller
home st
  = do style <- showURL Stylesheet
       db    <- liftIO . query st $ ReadState
       ok . toResponse . Html.master style . Html.wordList $ db

word :: State -> String -> Controller
word st w
  = do style <- showURL Stylesheet
       db    <- liftIO . query st $ ReadState
       ok . toResponse . Html.master style . Html.word $ w' db
    where w' db = Set.findMin . Set.filter p $ DB.words db
          p e   = DB.word e == w

stylesheet :: Controller
stylesheet
  = ok . setHeader "Content-Type" "text/css"
       . toResponse . renderCSS . runCSS $ Css.master
