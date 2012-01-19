module Test.HUnit.Diff where

import Preamble

import Data.Algorithm.Diff
import Prelude (lines, unlines)
import System.Console.ANSI
import Test.HUnit
import Text.Groom

(@?==) :: (Eq a, Show a) => a -> a -> Assertion
x @?== y =
    assertBool ('\n':msg) (x == y)
  where
    msg       = unlines $ fmt <$> getDiff (lines . groom $ x)
                                          (lines . groom $ y)
    fmt (B,s) =               ' ' : s
    fmt (F,s) = color Green $ '+' : s
    fmt (S,s) = color Red   $ '-' : s
    color c s = setSGRCode [SetColor Foreground Dull c]
                ++ s ++
                setSGRCode [Reset]
