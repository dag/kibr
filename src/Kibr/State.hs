{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, MonadComprehensions, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Event definitions for /acid-state/.
module Kibr.State
    ( -- * Application State Container
      AppState(..)
      -- ** Lenses
    , wordData
    , valueAtIx
      -- * Environments with state
    , query
    , update
    , update_
      -- * Event Definitions
      -- | These are exported for documentation purposes, but it's really
      -- the Event Methods ("Kibr.State#methods") that you use with /acid-state/.
    , searchKeyWords
    , lookupWordType
    , lookupWordDefinition
    , saveWordType
    , saveWordDefinition
    , listWordTypes
    , listWordDefinitions
    , importWords
      -- ** Event Methods
      -- | #methods#
    , SearchKeyWords(..)
    , LookupWordType(..)
    , LookupWordDefinition(..)
    , SaveWordType(..)
    , SaveWordDefinition(..)
    , ListWordTypes(..)
    , ListWordDefinitions(..)
    , ImportWords(..)
    )
  where

import qualified Data.Foldable as F
import qualified Data.IxSet    as IxSet
import qualified Data.Map      as Map
import qualified Data.Set      as Set

import Control.Lens          (Getter, (^.), (=%=), (%=), to, valueAt)
import Control.Lens.TH.Extra (makeLenses)
import Control.Monad         (void, forM_)
import Control.Monad.Reader  (asks)
import Control.Monad.State   (gets)
import Control.Monad.Trans   (MonadIO)
import Data.Acid             (AcidState, Update, Query, QueryEvent, UpdateEvent, EventResult, makeAcidic)
import Data.Acid.Advanced    (MethodState, query', update')
import Data.Default          (Default(def))
import Data.Has              (Has(fetch))
import Data.IxSet            (IxSet, Indexable, (@=))
import Data.SafeCopy         (deriveSafeCopy, base)
import Data.Set              (Set)
import Data.Typeable         (Typeable)
import Kibr.Data

data AppState = AppState
    { wordData' :: IxSet WordData
    }

makeLenses ''AppState

valueAtIx :: (Ord a, Typeable k, Typeable a, Indexable a)
        => k -> Getter (IxSet a) b (Maybe a) d
valueAtIx k = to (IxSet.getOne . IxSet.getEQ k)

instance Default AppState where def = AppState IxSet.empty

query :: (QueryEvent e, MonadIO m, Has (AcidState (MethodState e)) m)
      => e -> m (EventResult e)
query ev = do
    st <- fetch
    query' st ev

update :: (UpdateEvent e, MonadIO m, Has (AcidState (MethodState e)) m)
       => e -> m (EventResult e)
update ev = do
    st <- fetch
    update' st ev

update_ :: (UpdateEvent e, Functor m, MonadIO m, Has (AcidState (MethodState e)) m)
        => e -> m ()
update_ = void . update

searchKeyWords :: [KeyWord] -> Query AppState (Set Word)
searchKeyWords ks = do
    ix <- asks (^.wordData)
    let matches = foldr (IxSet.intersection . (ix @=)) ix ks
    return $ Set.map word' $ IxSet.toSet matches

-- | Look up the most current 'WordType' stored for a 'Word'.
lookupWordType :: Word -> Query AppState (Maybe WordType)
lookupWordType word = do
    wd <- asks (^.wordData.valueAtIx word)
    return [ t | WordData _ (Revision t _:_) _ <- wd ]

-- | Look up the most current 'WordDefinition' stored for a 'Word' in
-- a 'Language'.
lookupWordDefinition :: Word -> Language
                     -> Query AppState (Maybe WordDefinition)
lookupWordDefinition word language = do
    wd <- asks (^.wordData.valueAtIx word)
    return [ d | Revision d _:_ <- wd >>= (^.wordDefinition.valueAt language) ]

modifyWordData :: Word -> (WordData -> WordData) -> Update AppState ()
modifyWordData word modify =
    gets (^.wordData.valueAtIx word) >>= maybe (create >> update new) update
  where
    create = wordData %= IxSet.insert new
    new = WordData word def def
    update wd = void $ wordData %= IxSet.updateIx word (modify wd)

-- | Save a revised 'WordType' for a 'Word'.
saveWordType :: Word -> Revision WordType -> Update AppState ()
saveWordType word revision =
    modifyWordData word $ wordType =%= (revision:)

-- | Save a revised 'WordDefinition' for a 'Word' in a 'Language'.
saveWordDefinition :: Word -> Language -> Revision WordDefinition
                   -> Update AppState ()
saveWordDefinition word language revision =
    modifyWordData word $
      wordDefinition =%= Map.insertWith (++) language [revision]

-- | List all 'WordType' revisions for a 'Word'.
listWordTypes :: Word -> Query AppState (History WordType)
listWordTypes word = do
    wd <- asks (^.wordData.valueAtIx word)
    return $ F.concat $ fmap (^.wordType) wd

-- | List all 'WordDefinition' revisions for a 'Word' in a 'Language'.
listWordDefinitions :: Word -> Language
                    -> Query AppState (History WordDefinition)
listWordDefinitions word language = do
    wd <- asks (^.wordData.valueAtIx word)
    return $ F.concat $ wd >>= (^.wordDefinition.valueAt language)

-- | Import the data parsed from an XML export in one go.
importWords :: [(Language,[(Word,WordType,WordDefinition)])] -> Update AppState ()
importWords dict =
    void $ forM_ dict $ \(language,words) ->
      forM_ words $ \(word,wordType,wordDefinition) ->
        do saveWordType word (Revision wordType SystemUser)
           saveWordDefinition word language (Revision wordDefinition SystemUser)

deriveSafeCopy 0 'base ''AppState
makeAcidic ''AppState [ 'searchKeyWords
                      , 'lookupWordType
                      , 'lookupWordDefinition
                      , 'saveWordType
                      , 'saveWordDefinition
                      , 'listWordTypes
                      , 'listWordDefinitions
                      , 'importWords
                      ]
