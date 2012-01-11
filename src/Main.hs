module Main where

import Preamble

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (IO, putStrLn)

import qualified Kibr.Http as Http
import qualified Kibr.Irc  as Irc
import qualified Kibr.Test as Test
import qualified Kibr.Xml  as Xml

main :: IO ()
main = run =<< getArgs

run :: [String] -> IO ()
run ("http":args)   = Http.run args
run ("irc":args)    = Irc.run args
run ("import":args) = Xml.run args
run ("test":args)   = Test.run args
run _               =
  do
    putStrLn "usage: kibr <http|import|test> [args]"
    exitFailure
