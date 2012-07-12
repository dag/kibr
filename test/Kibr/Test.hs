{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}

module Kibr.Test (tests) where

import Kibr.State hiding (query, update)

import qualified Data.Set             as Set
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence        as Seq
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT

import Control.Monad     (void, forM_)
import Data.Acid         (update, query)
import Data.Acid.Memory  (openMemoryState)
import Data.Default      (def)
import Data.Packable     (pack, unpack, fromList)
import Data.Word         (Word8)
import Kibr.Data
import Kibr.Fixture
import Kibr.XML
import Test.HUnit        (Assertion, (@?=))
import Text.XML.HXT.Core

import Test.Framework                       (Test)
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH                    (testGroupGenerator)
import Test.QuickCheck.Instances            ()

tests :: Test
tests = $testGroupGenerator

prop_BS_pack :: [Word8] -> Bool
prop_BS_pack ws = unpack (pack ws :: BS.ByteString) == ws

prop_BS_unpack :: BS.ByteString -> Bool
prop_BS_unpack bs = pack (unpack bs) == bs

prop_LBS_pack :: [Word8] -> Bool
prop_LBS_pack ws = unpack (pack ws :: LBS.ByteString) == ws

prop_LBS_unpack :: LBS.ByteString -> Bool
prop_LBS_unpack bs = pack (unpack bs) == bs

prop_Seq_pack :: [Int] -> Bool
prop_Seq_pack lst = unpack (pack lst :: Seq.Seq Int) == lst

prop_Seq_unpack :: Seq.Seq Int -> Bool
prop_Seq_unpack seq = pack (unpack seq) == seq

prop_T_pack :: String -> Bool
prop_T_pack str = unpack (pack str :: T.Text) == str

prop_T_unpack :: T.Text -> Bool
prop_T_unpack txt = pack (unpack txt) == txt

prop_LT_pack :: String -> Bool
prop_LT_pack str = unpack (pack str :: LT.Text) == str

prop_LT_unpack :: LT.Text -> Bool
prop_LT_unpack txt = pack (unpack txt) == txt

case_getWordType :: Assertion
case_getWordType =
    forM_ fixtures $ \Fixture{..} ->
      do wordType <- runX $ fXML >>> getWordType
         wordType @?= [fWordType]

case_getWordDefinition :: Assertion
case_getWordDefinition =
    forM_ fixtures $ \Fixture{..} ->
      do wordDef <- runX $ fXML >>> getWordDefinition
         wordDef @?= [fWordDefinition]

case_getWord :: Assertion
case_getWord =
    forM_ fixtures $ \Fixture{..} ->
      do word <- runX $ fXML >>> getWord
         word @?= [(fWord, fWordType, fWordDefinition)]

case_readDictionary :: Assertion
case_readDictionary = do
    [(language, words)] <- runX $ dictionary >>> readDictionary
    language @?= English UnitedStates
    words @?= [(fWord, fWordType, fWordDefinition)]
  where
    Fixture{..} = ba'e

ba'eWordData :: WordData
ba'eWordData =
    WordData fWord (hist fWordType) trans
  where
    Fixture{..} = ba'e
    hist a      = [Revision a SystemUser]
    trans       = fromList [(English UnitedStates, hist fWordDefinition)]

case_LookupWordType :: Assertion
case_LookupWordType = do
    acid <- openMemoryState $ AppState (fromList [ba'eWordData])
    wt <- query acid $ LookupWordType "ba'e"
    wt @?= Just (ParticleWord Set.empty "BAhE")

case_LookupWordDefinition :: Assertion
case_LookupWordDefinition = do
    acid <- openMemoryState $ AppState (fromList [ba'eWordData])
    wd <- query acid $ LookupWordDefinition "ba'e" (English UnitedStates)
    wd @?= Just "forethought emphasis indicator."

case_SaveWordType :: Assertion
case_SaveWordType = do
    acid <- openMemoryState def
    void $ update acid $ SaveWordType "ba'e" rev
    wt <- query acid $ LookupWordType "ba'e"
    wt @?= Just (ParticleWord Set.empty "BAhE")
  where
    rev = Revision (ParticleWord Set.empty "BAhE") SystemUser

case_SaveWordDefinition :: Assertion
case_SaveWordDefinition = do
    acid <- openMemoryState def
    void $ update acid $ SaveWordDefinition "ba'e" (English UnitedStates) rev
    wd <- query acid $ LookupWordDefinition "ba'e" (English UnitedStates)
    wd @?= Just "forethought emphasis indicator."
  where
    rev = Revision "forethought emphasis indicator." SystemUser

case_ListWordTypes :: Assertion
case_ListWordTypes = do
    acid <- openMemoryState def
    void $ update acid $ SaveWordType "donri" (rev $ RootWord Set.empty ExperimentalRoot)
    void $ update acid $ SaveWordType "donri" (rev $ fWordType donri)
    hist <- query acid $ ListWordTypes "donri"
    hist @?= [rev $ fWordType donri, rev $ RootWord Set.empty ExperimentalRoot]
  where
    rev a = Revision a SystemUser

case_ListWordDefinitions :: Assertion
case_ListWordDefinitions = do
    acid <- openMemoryState def
    void $ update acid $ SaveWordDefinition "sferies" (English UnitedStates) (rev "Sweden")
    void $ update acid $ SaveWordDefinition "sferies" (English UnitedStates) (rev "Sweden.")
    hist <- query acid $ ListWordDefinitions "sferies" (English UnitedStates)
    hist @?= [rev "Sweden.", rev "Sweden"]
  where
    rev a = Revision a SystemUser
