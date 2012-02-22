module Main where

import Preamble
import Prelude (putStrLn, foldl)

import Control.Exception     (bracket)
import Data.Acid             (openLocalState)
import Data.Acid.Local       (createArchive, createCheckpointAndClose)
import Data.Kibr.Configuration
import Data.Kibr.State
import System.Console.GetOpt
import System.Environment    (getArgs)
import System.Exit           (exitFailure)
import System.IO             (hPutStrLn, hPutStr, stderr)
import Text.Groom            (groom)

import qualified Network.Kibr.Http as Http
import qualified Network.Kibr.Irc  as Irc
import qualified Text.Kibr.Xml     as Xml

#if DEVELOPMENT
import qualified Test.Kibr         as Test
#endif

main :: IO ()
main = run =<< getArgs

run :: [String] -> IO ()
#if DEVELOPMENT
run ("test":args) = Test.run args
#endif
run args =
  case getOpt RequireOrder options args of
    (fs,as,[]) -> do
      let config = foldl (flip id) def fs
      case as of
        "http":_        -> withState . Http.run $ toHappstackConf config
        "irc":_         -> withState . Irc.run $ toBotConf config
        "import":args'  -> withState . Xml.run $ args'
        "show-config":_ -> putStrLn $ groom config
        []              -> usage
        args'           -> do
          hPutStrLn stderr $ "invalid arguments: " ++ show args'
          usage
    (_,_,es) -> do
      mapM_ (hPutStr stderr) es
      usage
  where
    usage = do
      hPutStr stderr $ usageInfo header options
      exitFailure
#if DEVELOPMENT
    header = "usage: kibr [opts] <http|irc|import|show-config|test> [args]"
#else
    header = "usage: kibr [opts] <http|irc|import|show-config> [args]"
#endif

withState :: (Acid -> IO a) -> IO a
withState =
    bracket (openLocalState def) $ \st -> do
      createArchive st
      createCheckpointAndClose st
