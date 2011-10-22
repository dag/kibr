module Kibr.Http where

import Control.Monad (msum)
import Happstack.Server
import Language.CSS

import Kibr.Css  as Css
import Kibr.Data
import Kibr.Html as Html
import Kibr.Xml (mkDb)


runHttp :: String -> [String] -> IO ()
runHttp file args =
  case parseConfig args of
    Left errors  -> mapM_ putStrLn errors
    Right config -> do
        db <- mkDb file
        simpleHTTP config $ msum
            [ nullDir >> index db
            , dir "master.css" stylesheet
            ]


index :: [Word] -> ServerPartT IO Response
index db =
    ok . toResponse . Html.master . wordList $
        [w | w <- db, Root _ _ <- [shape w]]


stylesheet :: ServerPartT IO Response
stylesheet =
    ok . setHeader "content-type" "text/css"
       . toResponse . renderCSS . runCSS $ Css.master
