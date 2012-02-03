module Test.Kibr.Http where

import Preamble

import Data.Acid.Memory
import Data.Text.Lazy.Encoding        (decodeUtf8)
import Happstack.Server
import Happstack.Server.Test          (mkRequest)
import Test.Framework                 (Test)
import Test.Framework.Providers.HUnit
import Test.Framework.TH              (testGroupGenerator)
import Test.HUnit                     (Assertion, (@?=))
import Test.Kibr.Fixture
import Text.XML.HXT.Core

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy       as LT
import qualified Network.Kibr.Http    as Http

tests :: Test
tests = $testGroupGenerator

get :: String -> IO Response
get url =
  do
    st <- openMemoryState fixtures
    rq <- mkRequest url
    simpleHTTP'' (Http.master st) rq

parseHtml :: LB.ByteString -> IOStateArrow s b XmlTree
parseHtml =
    readString config . LT.unpack . decodeUtf8
  where
    config = [withParseHTML yes, withWarnings no]

getTitle :: ArrowXml a => a XmlTree String
getTitle = deep (hasName "title") >>> getChildren >>> getText

case_root_redirects :: Assertion
case_root_redirects =
  do
    rs@Response{..} <- get "/"
    rsCode @?= 303
    getHeader "Location" rs @?= Just "/Dictionary/English/Home"

case_title_English_i18n :: Assertion
case_title_English_i18n =
  do
    Response{..} <- get "/Dictionary/English/Home"
    [title]      <- runX $ parseHtml rsBody >>> getTitle
    (LT.strip . LT.pack) title @?= "Lojban Dictionary"

case_title_Lojban_i18n :: Assertion
case_title_Lojban_i18n =
  do
    Response{..} <- get "/Dictionary/Lojban/Home"
    [title]      <- runX $ parseHtml rsBody >>> getTitle
    (LT.strip . LT.pack) title @?= "vlaste fu la lojban"
