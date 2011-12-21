module Main where

import Preamble

import System.Environment (getArgs)
import System.IO (IO)

import Kibr.Http (runHttp)
import Kibr.Xml  (runImport)
import Kibr.Test (runTest)

main :: IO ()
main = run =<< getArgs

run :: [String] -> IO ()
run ("--test":args)   = runTest args
run ("--import":args) = runImport args
run args              = runHttp args
