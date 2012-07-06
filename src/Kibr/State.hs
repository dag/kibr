{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

-- | Event definitions for /acid-state/.
module Kibr.State
    ( -- * Application State Container
      AppState(AppState)
      -- * Event Definitions
      -- | These are exported for documentation purposes, but it's really
      -- the Event Methods ("Kibr.State#methods") that you use with /acid-state/.
    , lookupWordType
    , lookupWordDefinition
    , saveWordType
    , saveWordDefinition
    , listWordTypes
    , listWordDefinitions
      -- ** Event Methods
      -- | #methods#
    , LookupWordType(..)
    , LookupWordDefinition(..)
    , SaveWordType(..)
    , SaveWordDefinition(..)
    , ListWordTypes(..)
    , ListWordDefinitions(..)
    )
  where

import Prelude hiding (id, (.))

import qualified Data.Foldable as F
import qualified Data.IxSet    as IxSet
import qualified Data.Map      as Map

import Control.Category     (id, (.))
import Control.Monad        (void, when)
import Control.Monad.Reader (MonadReader, asks)
import Data.Acid            (Update, Query, makeAcidic)
import Data.Default         (Default(def))
import Data.IxSet           (IxSet, (@=))
import Data.Lens            (Lens, (%=), (^%=), (^.), getL, mapLens, access)
import Data.Lens.Template   (makeLens)
import Data.SafeCopy        (deriveSafeCopy, base)
import Kibr.Data

data AppState = AppState
    { _wordData :: IxSet WordData
    }

makeLens ''AppState

instance Default AppState where def = AppState IxSet.empty

summon :: MonadReader a m => Lens a b -> m b
summon = asks . getL

-- | Look up the most current 'WordType' stored for a 'Word'.
lookupWordType :: Word -> Query AppState (Maybe WordType)
lookupWordType word = do
    ix <- summon wordData
    return $ do WordData _ (Revision wt _:_) _ <- IxSet.getOne (ix @= word)
                return wt

-- | Look up the most current 'WordDefinition' stored for a 'Word' in
-- a 'Language'.
lookupWordDefinition :: Word -> Language
                     -> Query AppState (Maybe WordDefinition)
lookupWordDefinition word language = do
    ix <- summon wordData
    return $ do wd <- IxSet.getOne (ix @= word)
                Revision def _:_ <- wd^.(mapLens language . wordDefinition)
                return def

modifyWordData :: Word -> (WordData -> WordData) -> Update AppState ()
modifyWordData word modify = do
    ix <- access wordData
    case IxSet.getOne (ix @= word) of
      Nothing -> create >> update new
      Just wd -> update wd
  where
    create = wordData %= IxSet.insert new
    new = WordData word def def
    update wd = void $ wordData %= IxSet.updateIx word (modify wd)

-- | Save a revised 'WordType' for a 'Word'.
saveWordType :: Word -> Revision WordType -> Update AppState ()
saveWordType word revision =
    modifyWordData word $ wordType ^%= (revision:)

-- | Save a revised 'WordDefinition' for a 'Word' in a 'Language'.
saveWordDefinition :: Word -> Language -> Revision WordDefinition
                   -> Update AppState ()
saveWordDefinition word language revision =
    modifyWordData word $
      wordDefinition ^%= Map.insertWith (++) language [revision]

-- | List all 'WordType' revisions for a 'Word'.
listWordTypes :: Word -> Query AppState (History WordType)
listWordTypes word = do
    ix <- summon wordData
    return $ F.concat $ do wd <- IxSet.getOne (ix @= word)
                           return $ wd^.wordType

-- | List all 'WordDefinition' revisions for a 'Word' in a 'Language'.
listWordDefinitions :: Word -> Language
                    -> Query AppState (History WordDefinition)
listWordDefinitions word language = do
    ix <- summon wordData
    return $ F.concat $ do wd <- IxSet.getOne (ix @= word)
                           wd^.(mapLens language . wordDefinition)

deriveSafeCopy 0 'base ''AppState
makeAcidic ''AppState [ 'lookupWordType
                      , 'lookupWordDefinition
                      , 'saveWordType
                      , 'saveWordDefinition
                      , 'listWordTypes
                      , 'listWordDefinitions
                      ]
