module Main (main) where

import Kibr.Run (kibr)
import Kibr.CLI (baseConfig)

main :: IO ()
main = kibr baseConfig
