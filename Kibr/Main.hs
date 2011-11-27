module Main where

import System.Environment (getArgs)

import Kibr.Http (runHttp)
import Kibr.Test (runTest)

main :: IO ()
main = run =<< getArgs

run :: [String] -> IO ()
run ("--test":args) = runTest args
run (file:args)     = runHttp file args
run _               = putStrLn "error: invalid command-line"
