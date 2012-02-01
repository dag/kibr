module Test.Kibr where

import Preamble

import Data.Acid.Memory
import Data.IxSet as Ix
import Data.Map   as Map
import Data.Set   as Set
import Data.Text.Lazy.Encoding (decodeUtf8)
import Happstack.Server
import System.Environment (withArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit hiding (State)
import Test.HUnit.Diff
import Text.XML.HXT.Core

import Data.Kibr.Grammar
import Data.Kibr.Language
import Data.Kibr.Revision
import Data.Kibr.State
import Data.Kibr.Word
import Happstack.Server.Test
import Language.CSS.YUI
import Text.Kibr.Xml (readDictionary)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy       as LT
import qualified Network.Kibr.Http    as Http

run :: [String] -> IO ()
run args = withArgs args $defaultMainGenerator

fixtures :: State
fixtures =
  let
    def d n = Map.fromList [(English, rev d n)]
    rev d n = [Revision (Definition d n) (Just "Imported")]
  in
    State $ Ix.fromList
      [ Word "ba'e" (Particle Set.empty BAhE) $
          def "forethought emphasis indicator." Nothing
      , Word "la'oi" (ProposedParticle Set.empty) .
          def "single-word non-Lojban name." $
            Just "See also {la'o}, {zo'oi}."
      , Word "donri" (Root (Set.fromList ["dor", "do'i"]) False) .
          def "$x_{1}$ is the daytime of day $x_{2}$." $
            Just "See also {nicte}, {djedi}, {tcika}."
      , Word "kibro" (Root Set.empty True) $
          def "$x_1$ pertains to the internet in aspect $x_2$." Nothing
      , Word "jbobau" Compound $
          def "$l_1=b_1$ is Lojban used by $b_2$." Nothing
      , Word "pinpedi" Loan $
          def "$x_1$ is a seal of species $x_2$." Nothing
      , Word "sferies" Name $ def "Sweden." Nothing
      , Word "ba'unai" Cluster $ def "discursive: understatement." Nothing
      ]

get :: String -> IO Response
get url =
  do
    st <- openMemoryState fixtures
    rq <- mkRequest url
    simpleHTTP'' (Http.master st) rq

parseHtml :: LB.ByteString -> IOStateArrow s b XmlTree
parseHtml = readString [withParseHTML yes] . LT.unpack . decodeUtf8

getTitle :: ArrowXml a => a XmlTree String
getTitle = deep (hasName "title") >>> getChildren >>> getText

case_readDictionary :: Assertion
case_readDictionary =
  do
    dictionary <- readDictionary English "data/fixtures.xml"
    dictionary @?== fixtures

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

case_root_redirects :: Assertion
case_root_redirects =
  do
    rs@Response{..} <- get "/"
    rsCode @?= 303
    getHeader "Location" rs @?= Just "/English/"

case_title_English_i18n :: Assertion
case_title_English_i18n =
  do
    Response{..} <- get "/English/"
    [title]      <- runX $ parseHtml rsBody >>> getTitle
    (LT.strip . LT.pack) title @?= "Lojban Dictionary"

case_title_Lojban_i18n :: Assertion
case_title_Lojban_i18n =
  do
    Response{..} <- get "/Lojban/"
    [title]      <- runX $ parseHtml rsBody >>> getTitle
    (LT.strip . LT.pack) title @?= "vlaste fu la lojban"
