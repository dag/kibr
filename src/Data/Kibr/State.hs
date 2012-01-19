module Data.Kibr.State where

import Preamble

import Control.Monad.Reader (ask, asks)
import Control.Monad.State  (put)

import Data.Acid
import Data.IxSet as Ix
import Data.Lens
import Data.Lens.IxSet
import Data.Lens.Template
import Data.SafeCopy

import Data.Kibr as DB

data State
  = State { _words :: IxSet Word }
  deriving (Eq, Show, Data, Typeable)

deriveSafeCopy 0 'base ''State

makeLens ''State

type Acid = AcidState State

askL :: Lens a b -> Query a b
askL = asks . getL

writeState :: State -> Update State ()
writeState = put

readState :: Query State State
readState = ask

lookupWord :: Text -> Query State (Maybe Word)
lookupWord w = askL $ ixLens (ByWord w) . words

reviseWord :: Text -> Language -> Revision Definition -> Update State ()
reviseWord w l r =
  do
    word . words %= map (lang . definitions ^%= map (r:))
    pure ()
  where
    word = ixLens $ ByWord w
    lang = mapLens l

makeAcidic ''State
  [ 'writeState
  , 'readState
  , 'lookupWord
  , 'reviseWord
  ]
