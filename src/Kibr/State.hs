{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses, Rank2Types, TemplateHaskell, TypeFamilies #-}

-- | Event definitions for /acid-state/.
module Kibr.State
    ( -- * Application State Container
      AppState(AppState)
      -- * Environments with state
    , HasAcidState(..)
    , query
    , update
    , update_
      -- * Event Definitions
      -- | These are exported for documentation purposes, but it's really
      -- the Event Methods ("Kibr.State#methods") that you use with /acid-state/.
    , lookupWordType
    , lookupWordDefinition
    , saveWordType
    , saveWordDefinition
    , listWordTypes
    , listWordDefinitions
    , importWords
      -- ** Event Methods
      -- | #methods#
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

import Control.Monad        (void, forM_)
import Control.Monad.Reader (asks)
import Control.Monad.State  (gets)
import Control.Monad.Trans  (MonadIO)
import Data.Acid            (AcidState, Update, Query, QueryEvent, UpdateEvent, EventResult, makeAcidic)
import Data.Acid.Advanced   (MethodState, query', update')
import Data.Default         (Default(def))
import Data.IxSet           (IxSet, Indexable)
import Data.SafeCopy        (deriveSafeCopy, base)
import Data.Typeable        (Typeable)
import Kibr.Data
import Lens.Family2         (GetterFamily, getting, (^%=), (^.))
import Lens.Family2.State   ((%=))
import Lens.Family2.Stock   (mapL)
import Lens.Family2.TH      (mkLenses)

data AppState = AppState
    { _wordData :: IxSet WordData
    }

mkLenses ''AppState

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

getOneL :: (Ord a, Typeable k, Typeable a, Indexable a)
        => k -> GetterFamily (IxSet a) a' (Maybe a) b'
getOneL k = getting (IxSet.getOne . IxSet.getEQ k)

-- | Look up the most current 'WordType' stored for a 'Word'.
lookupWordType :: Word -> Query AppState (Maybe WordType)
lookupWordType word = do
    wd <- asks (^.wordData.wordL)
    return $ do WordData _ (Revision wt _:_) _ <- wd
                return wt
  where
    wordL = getOneL word

-- | Look up the most current 'WordDefinition' stored for a 'Word' in
-- a 'Language'.
lookupWordDefinition :: Word -> Language
                     -> Query AppState (Maybe WordDefinition)
lookupWordDefinition word language = do
    wd <- asks (^.wordData.wordL)
    return $ do Revision def _:_ <- wd >>= (^.wordDefinition.langL)
                return def
  where
    wordL = getOneL word
    langL = mapL language

modifyWordData :: Word -> (WordData -> WordData) -> Update AppState ()
modifyWordData word modify =
    gets (^.wordData.wordL) >>= maybe (create >> update new) update
  where
    wordL = getOneL word
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
    wd <- asks (^.wordData.wordL)
    return $ F.concat $ fmap (^.wordType) wd
  where
    wordL = getOneL word

-- | List all 'WordDefinition' revisions for a 'Word' in a 'Language'.
listWordDefinitions :: Word -> Language
                    -> Query AppState (History WordDefinition)
listWordDefinitions word language = do
    wd <- asks (^.wordData.wordL)
    return $ F.concat $ wd >>= (^.wordDefinition.langL)
  where
    wordL = getOneL word
    langL = mapL language

-- | Import the data parsed from an XML export in one go.
importWords :: [(Language,[(Word,WordType,WordDefinition)])] -> Update AppState ()
importWords dict =
    void $ forM_ dict $ \(language,words) ->
      forM_ words $ \(word,wordType,wordDefinition) ->
        do saveWordType word (Revision wordType SystemUser)
           saveWordDefinition word language (Revision wordDefinition SystemUser)

deriveSafeCopy 0 'base ''AppState
makeAcidic ''AppState [ 'lookupWordType
                      , 'lookupWordDefinition
                      , 'saveWordType
                      , 'saveWordDefinition
                      , 'listWordTypes
                      , 'listWordDefinitions
                      , 'importWords
                      ]
