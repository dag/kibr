{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Kibr.State where

import Preamble

import Control.Monad.Reader (ask)
import Control.Monad.State  (put)

import Data.Acid
import Data.HiggsSet as Higgs
import Data.Kibr.Word
import Data.Lens.Reader
import Data.Lens.Template
import Data.SafeCopy

import qualified Data.Text as T

instance (SafeCopy a, Indexable a, Index i, IndexOf a ~ i)
         => SafeCopy (HiggsSet a i) where
  putCopy = contain . safePut . Higgs.toList
  getCopy = contain $ Higgs.fromList <$> safeGet

instance (Eq a, Indexable a, Index i, IndexOf a ~ i)
         => Eq (HiggsSet a i) where
  a == b = Higgs.toList a == Higgs.toList b

instance (Show a, Indexable a, Index i, IndexOf a ~ i)
         => Show (HiggsSet a i) where
  show h = "fromList " ++ show (Higgs.toList h)

data State = State { _words :: HiggsSet Word WordIndex }
  deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''State

makeLens ''State

type Acid = AcidState State

writeState :: State -> Update State ()
writeState = put

readState :: Query State State
readState = ask

lookupWord :: Text -> Query State (Maybe Word)
lookupWord w =
  do
    ws <- askL words
    pure . lookup (ByWord w') $ ws
  where
    w' = T.replace "h" "'" . T.replace "." T.empty $ w

makeAcidic ''State
  [ 'writeState
  , 'readState
  , 'lookupWord
  ]
