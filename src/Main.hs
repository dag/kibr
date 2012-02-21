module Main where

import Preamble
import Prelude (foldl)

import Control.Exception     (bracket)
import Data.Acid             (openLocalState)
import Data.Acid.Local       (createArchive, createCheckpointAndClose)
import Data.Kibr.State
import System.Console.GetOpt
import System.Environment    (getArgs)
import System.Exit           (exitFailure)
import System.IO             (hPutStrLn, hPutStr, stderr)

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
#if DEVELOPMENT
run ("test":args) = Test.run args
#endif
run args =
  case getOpt RequireOrder Config.options args of
    (fs,as,[]) -> do
      let config = foldl (flip id) Config.master fs
      case as of
        ("http":_)       -> withState . Http.run $ Config.http config
        ("irc":_)        -> withState . Irc.run $ Config.irc config
        ("import":args') -> withState . Xml.run $ args'
        []               -> usage
        args'            -> do
          hPutStrLn stderr $ "invalid arguments: " ++ show args'
          usage
    (_,_,es) -> do
      mapM_ (hPutStr stderr) es
      usage
  where
    usage = do
      hPutStr stderr $ usageInfo header Config.options
      exitFailure
#if DEVELOPMENT
    header = "usage: kibr [opts] <http|irc|import|test> [args]"
#else
    header = "usage: kibr [opts] <http|irc|import> [args]"
#endif

withState :: (Acid -> IO a) -> IO a
withState =
    bracket (openLocalState $ State Ix.empty) $ \st -> do
      createArchive st
      createCheckpointAndClose st
