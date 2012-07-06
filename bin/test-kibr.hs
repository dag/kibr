module Main (main) where

import Kibr.Test      (tests)
import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain [tests]
