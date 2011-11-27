module Kibr.Http where

import Control.Monad (msum)
import Happstack.Server
import Language.CSS

import Kibr.Css  as Css
import Kibr.Data
import Kibr.Html as Html
import Kibr.Xml (readDictionary)

runHttp :: String -> [String] -> IO ()
runHttp file args =
  case parseConfig args of
    Left errors  -> mapM_ putStrLn errors
    Right config -> do
        dict <- readDictionary English file
        simpleHTTP config $ msum
            [ nullDir >> index dict
            , dir "master.css" stylesheet
            ]

index :: Dictionary -> ServerPartT IO Response
index dict =
    ok . toResponse . Html.master . wordList $ dict

stylesheet :: ServerPartT IO Response
stylesheet =
    ok . setHeader "content-type" "text/css"
       . toResponse . renderCSS . runCSS $ Css.master
