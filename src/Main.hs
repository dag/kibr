module Main where

import Preamble
import Prelude (error)

import Control.Exception  (bracket)
import Data.Acid          (openLocalState)
import Data.Acid.Local    (createArchive, createCheckpointAndClose)
import Data.Kibr.State
import System.Environment (getArgs)

import qualified Data.IxSet              as Ix
import qualified Data.Kibr.Configuration as Config
import qualified Network.Kibr.Http       as Http
import qualified Network.Kibr.Irc        as Irc
import qualified Text.Kibr.Xml           as Xml

#if DEVELOPMENT
import qualified Test.Kibr         as Test
#endif

main :: IO ()
main = run =<< getArgs

run :: [String] -> IO ()
run ("http":_)      = withState . Http.run $ Config.http Config.master
run ("irc":_)       = withState . Irc.run $ Config.irc Config.master
run ("import":args) = withState $ Xml.run args
#if DEVELOPMENT
run ("test":args)   = Test.run args
run _               = error "usage: kibr <http|irc|import|test> [args]"
#else
run _               = error "usage: kibr <http|irc|import> [args]"
#endif

withState :: (Acid -> IO a) -> IO a
withState =
    bracket (openLocalState $ State Ix.empty) $ \st -> do
      createArchive st
      createCheckpointAndClose st
