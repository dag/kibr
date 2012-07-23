module Main (main) where

import Data.Conf (conf)
import Kibr.Run  (kibr)

main :: IO ()
main = kibr conf
