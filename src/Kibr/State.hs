{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Event definitions for /acid-state/.
module Kibr.State
    ( -- * Application State Container
      AppState(..)
      -- ** Lenses
    , wordData
      -- * Environments with state
    , HasAcidState(..)
    , query
    , update
    , update_
      -- * Event Definitions
      -- | These are exported for documentation purposes, but it's really
      -- the Event Methods ("Kibr.State#methods") that you use with /acid-state/.
    , completeWords
    , searchKeyWords
    , lookupWordType
    , lookupWordDefinition
    , lookupWords
    , saveWordType
    , saveWordDefinition
    , listWordTypes
    , listWordDefinitions
    , importWords
      -- ** Event Methods
      -- | #methods#
    , CompleteWords(..)
    , SearchKeyWords(..)
    , LookupWordType(..)
    , LookupWordDefinition(..)
    , LookupWords(..)
    , SaveWordType(..)
    , SaveWordDefinition(..)
    , ListWordTypes(..)
    , ListWordDefinitions(..)
    , ImportWords(..)
    )
  where

import qualified Data.Foldable   as F
import qualified Data.IxSet      as IxSet
import qualified Data.IxSet.Lens as IxSet
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.Text       as Text

import Control.Applicative   ((<$>), (<*>))
import Control.Lens          ((^.), (%~), (%=), at)
import Control.Lens.TH.Extra (makeLenses)
import Control.Monad         (void, forM, forM_)
import Control.Monad.Reader  (asks)
import Control.Monad.State   (gets)
import Control.Monad.Trans   (MonadIO)
import Data.Acid             (AcidState, Update, Query, QueryEvent, UpdateEvent, EventResult, makeAcidic)
import Data.Acid.Advanced    (MethodState, query', update')
import Data.Default          (Default(def))
import Data.IxSet            (IxSet, (@=))
import Data.Maybe            (catMaybes)
import Data.SafeCopy         (deriveSafeCopy, base)
import Data.Set              (Set)
import Data.Text             (Text)
import Kibr.Data

data AppState = AppState
    { wordData' :: IxSet WordData
    }

makeLenses ''AppState

instance Default AppState where def = AppState IxSet.empty

class HasAcidState m st where
    getAcidState :: m (AcidState st)

query :: (QueryEvent e, MonadIO m, HasAcidState m (MethodState e))
      => e -> m (EventResult e)
query ev = do
    st <- getAcidState
    query' st ev

update :: (UpdateEvent e, MonadIO m, HasAcidState m (MethodState e))
       => e -> m (EventResult e)
update ev = do
    st <- getAcidState
    update' st ev

update_ :: (UpdateEvent e, Functor m, MonadIO m, HasAcidState m (MethodState e))
        => e -> m ()
update_ = void . update

completeWords :: Text -> Query AppState (Set Word)
completeWords txt = do
    ix <- asks (^.wordData)
    return $ Set.map word' $ Set.filter complete $ IxSet.toSet ix
  where
    complete WordData{..} = txt `Text.isPrefixOf` unWord word'

searchKeyWords :: [KeyWord] -> Query AppState (Set Word)
searchKeyWords ks = do
    ix <- asks (^.wordData)
    let matches = foldr (IxSet.intersection . (ix @=)) ix ks
    return $ Set.map word' $ IxSet.toSet matches

-- | Look up the most current 'WordType' stored for a 'Word'.
lookupWordType :: Word -> Query AppState (Maybe WordType)
lookupWordType word = do
    wd <- asks (^.wordData.IxSet.at word)
    return [ t | WordData _ (Revision t _:_) _ <- wd ]

-- | Look up the most current 'WordDefinition' stored for a 'Word' in
-- a 'Language'.
lookupWordDefinition :: Word -> Language
                     -> Query AppState (Maybe WordDefinition)
lookupWordDefinition word language = do
    wd <- asks (^.wordData.IxSet.at word)
    return [ d | Revision d _:_ <- wd >>= (^.wordDefinition.at language) ]

lookupWords :: [Word] -> Language -> Query AppState [(Word,WordType,WordDefinition)]
lookupWords words language = do
    ms <- forM words $ \word ->
      do typ <- lookupWordType word
         def <- lookupWordDefinition word language
         return $ (word,,) <$> typ <*> def
    return $ catMaybes ms

modifyWordData :: Word -> (WordData -> WordData) -> Update AppState ()
modifyWordData word modify =
    gets (^.wordData.IxSet.at word) >>= maybe (create >> update new) update
  where
    create = wordData %= IxSet.insert new
    new = WordData word def def
    update wd = void $ wordData %= IxSet.updateIx word (modify wd)

-- | Save a revised 'WordType' for a 'Word'.
saveWordType :: Word -> Revision WordType -> Update AppState ()
saveWordType word revision =
    modifyWordData word $ wordType %~ (revision:)

-- | Save a revised 'WordDefinition' for a 'Word' in a 'Language'.
saveWordDefinition :: Word -> Language -> Revision WordDefinition
                   -> Update AppState ()
saveWordDefinition word language revision =
    modifyWordData word $
      wordDefinition %~ Map.insertWith (++) language [revision]

-- | List all 'WordType' revisions for a 'Word'.
listWordTypes :: Word -> Query AppState (History WordType)
listWordTypes word = do
    wd <- asks (^.wordData.IxSet.at word)
    return $ F.concat $ fmap (^.wordType) wd

-- | List all 'WordDefinition' revisions for a 'Word' in a 'Language'.
listWordDefinitions :: Word -> Language
                    -> Query AppState (History WordDefinition)
listWordDefinitions word language = do
    wd <- asks (^.wordData.IxSet.at word)
    return $ F.concat $ wd >>= (^.wordDefinition.at language)

-- | Import the data parsed from an XML export in one go.
importWords :: [(Language,[(Word,WordType,WordDefinition)])] -> Update AppState ()
importWords dict =
    void $ forM_ dict $ \(language,words) ->
      forM_ words $ \(word,wordType,wordDefinition) ->
        do saveWordType word (Revision wordType SystemUser)
           saveWordDefinition word language (Revision wordDefinition SystemUser)

deriveSafeCopy 0 'base ''AppState
makeAcidic ''AppState [ 'completeWords
                      , 'searchKeyWords
                      , 'lookupWordType
                      , 'lookupWordDefinition
                      , 'lookupWords
                      , 'saveWordType
                      , 'saveWordDefinition
                      , 'listWordTypes
                      , 'listWordDefinitions
                      , 'importWords
                      ]
