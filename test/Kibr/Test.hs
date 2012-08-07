{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kibr.Test (tests) where

import Kibr.State hiding (query, update)

import qualified Data.IxSet as IxSet
import qualified Data.Map   as Map
import qualified Data.Set   as Set

import Control.Monad     (void, forM_)
import Data.Acid         (update, query)
import Data.Acid.Memory  (openMemoryState)
import Data.Default      (def)
import Kibr.Data
import Kibr.Fixture
import Kibr.XML
import Test.HUnit        (Assertion, (@?=))
import Text.XML.HXT.Core

import Test.Framework                       (Test)
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Providers.QuickCheck2 ()  -- (testProperty)
import Test.Framework.TH                    (testGroupGenerator)
import Test.QuickCheck.Instances            ()

tests :: Test
tests = $testGroupGenerator

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
    trans       = Map.fromList [(English UnitedStates, hist fWordDefinition)]

case_LookupWordType :: Assertion
case_LookupWordType = do
    acid <- openMemoryState $ AppState (IxSet.fromList [ba'eWordData])
    wt <- query acid $ LookupWordType "ba'e"
    wt @?= Just (ParticleWord Set.empty "BAhE")

case_LookupWordDefinition :: Assertion
case_LookupWordDefinition = do
    acid <- openMemoryState $ AppState (IxSet.fromList [ba'eWordData])
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
