module Test.Kibr.Xml where

import Data.Kibr.Language             (Language(English))
import Test.Framework                 (Test)
import Test.Framework.Providers.HUnit
import Test.Framework.TH              (testGroupGenerator)
import Test.HUnit                     (Assertion)
import Test.HUnit.Diff                ((@?==))
import Test.Kibr.Fixture
import Text.Kibr.Xml                  (readDictionary)

tests :: Test
tests = $testGroupGenerator

case_readDictionary :: Assertion
case_readDictionary =
  do
    dictionary <- readDictionary English "data/fixtures.xml"
    dictionary @?== fixtures
