module Test.Kibr.Css where

import Preamble

import Language.CSS.YUI                     (pxToPercent)
import Test.Framework                       (Test)
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH                    (testGroupGenerator)
import Test.HUnit                           (Assertion, (@?=))

import qualified Data.Text.Lazy as LT

tests :: Test
tests = $testGroupGenerator

case_pxToPercent_computed :: Assertion
case_pxToPercent_computed =
  do
    pxToPercent 9  @?= "69.2%"
    pxToPercent 28 @?= "215.4%"

prop_pxToPercent_starts_with_digit :: Int -> Bool
prop_pxToPercent_starts_with_digit px
  | px >= 0   = isDigit . LT.head . pxToPercent $ px
  | otherwise = LT.head (pxToPercent px) == '-'

prop_pxToPercent_ends_in_percent_sign :: Int -> Bool
prop_pxToPercent_ends_in_percent_sign px = LT.last (pxToPercent px) == '%'

prop_pxtoPercent_is_rounded :: Int -> Bool
prop_pxtoPercent_is_rounded px =
  case LT.split (== '.') (pxToPercent px) of
    [_, d] -> LT.length d == 2
    [_]    -> True
    _      -> False
