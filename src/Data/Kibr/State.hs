module Data.Kibr.State where

import Preamble

import Control.Monad.Reader (ask)
import Control.Monad.State  (put)

import Data.Acid
import Data.IxSet as Ix
import Data.Lens
import Data.Lens.IxSet
import Data.Lens.Reader
import Data.Lens.Template
import Data.SafeCopy

import Data.Kibr.Language
import Data.Kibr.Revision
import Data.Kibr.Word

import qualified Data.Text as T

data State
  = State { _words :: IxSet Word }
  deriving (Eq, Show, Data, Typeable)

deriveSafeCopy 0 'base ''State

makeLens ''State

type Acid = AcidState State

writeState :: State -> Update State ()
writeState = put

readState :: Query State State
readState = ask

lookupWord :: Text -> Query State (Maybe Word)
lookupWord w = askL $ ixLens (ByWord w') . words
  where
    w' = T.replace "h" "'" . T.replace "." T.empty $ w

lookupAffix :: Text -> Query State (Maybe Word)
lookupAffix a = askL $ ixLens (ByAffix a') . words
  where
    a' = T.replace "h" "'" a

reviseWord :: Text -> Language -> Revision Definition -> Update State ()
reviseWord w l r =
  do
    byWord . words %= map (lang . definitions ^%= map (r:))
    pure ()
  where
    byWord = ixLens $ ByWord w
    lang   = mapLens l

makeAcidic ''State
  [ 'writeState
  , 'readState
  , 'lookupWord
  , 'lookupAffix
  , 'reviseWord
  ]
