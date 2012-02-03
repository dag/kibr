module Data.Kibr.State where

import Preamble

import Control.Monad.Reader (ask)
import Control.Monad.State  (put)

import Data.Acid
import Data.HiggsSet as Higgs
import Data.Lens
import Data.Lens.Reader
import Data.Lens.Template
import Data.SafeCopy

import Data.Kibr.Language
import Data.Kibr.Revision
import Data.Kibr.Word

import qualified Data.Text as T

data State = State { _words :: HiggsSet Word WordIndex }

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
