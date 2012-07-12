module Main (main) where

import Data.Configurable (conf)
import Kibr.Run          (kibr)

main :: IO ()
main = kibr conf
