{-# LANGUAGE DeriveDataTypeable, Rank2Types, TemplateHaskell, TypeFamilies #-}

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

import qualified Data.Foldable as F
import qualified Data.IxSet    as IxSet
import qualified Data.Map      as Map

import Control.Monad        (void, when)
import Control.Monad.Reader (MonadReader, asks)
import Data.Acid            (Update, Query, makeAcidic)
import Data.Default         (Default(def))
import Data.IxSet           (IxSet, (@=))
import Data.SafeCopy        (deriveSafeCopy, base)
import Kibr.Data
import Lens.Family2         (Getter, (^%=), (^.))
import Lens.Family2.State   ((%=), access)
import Lens.Family2.Stock   (mapL)
import Lens.Family2.TH      (mkLenses)

data AppState = AppState
    { _wordData :: IxSet WordData
    }

mkLenses ''AppState

instance Default AppState where def = AppState IxSet.empty

summon :: MonadReader a m => Getter a b -> m b
summon l = asks (^.l)

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
                Revision def _:_ <- wd^.wordDefinition.langL
                return def
  where
    langL = mapL language

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
                           wd^.wordDefinition.langL
  where
    langL = mapL language

deriveSafeCopy 0 'base ''AppState
makeAcidic ''AppState [ 'lookupWordType
                      , 'lookupWordDefinition
                      , 'saveWordType
                      , 'saveWordDefinition
                      , 'listWordTypes
                      , 'listWordDefinitions
                      ]
