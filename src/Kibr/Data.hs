{-# LANGUAGE CPP, DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, RecordWildCards, TemplateHaskell #-}

-- | Application data model.
module Kibr.Data
    ( -- * Users
      User(..)
      -- * Languages
    , Language(..)
    , Region(..)
    , LanguageTag(..)
    , languages
    , languageTags
      -- * Revision Histories
    , History
    , Revision(..)
      -- * Dictionary
    , AffixForms
    , Affix(..)
    , ParticleClass(..)
    , RootWordStatus(..)
    , WordType(..)
    , WordTranslations
    , WordDefinition(..)
    , SearchWord(..)
    , KeyWord(..)
    , Word(..)
    , WordData(..)
      -- ** Lenses
    , word
    , wordType
    , wordDefinition
    )
  where

import qualified Data.Foldable     as F
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IxSet        as IxSet
import qualified Data.Map          as Map
import qualified Data.Text         as Text

#ifndef WINDOWS
import qualified Data.Text.ICU     as ICU
#endif

import Control.Lens                  (Lens)
import Control.Lens.TH.Extra         (makeLenses)
import Data.Hashable                 (Hashable)
import Data.HashMap.Lazy             (HashMap)
import Data.IxSet                    (Indexable, ixSet, ixFun)
import Data.Map                      (Map)
import Data.SafeCopy                 (SafeCopy, deriveSafeCopy, base)
import Data.Set                      (Set)
import Data.String                   (IsString(fromString))
import Data.Text                     (Text)
import Data.Typeable                 (Typeable)
import Text.InterpolatedString.Perl6 (ShowQ, qq)

#ifndef WINDOWS
import Data.Text.ICU                 (LocaleName(Locale), breaks, breakWord, brkBreak, brkStatus)
#endif


-- * Users
-- ***************************************************************************

data User = SystemUser deriving (Eq, Show, Ord)


-- * Languages
-- ***************************************************************************

data Language = Lojban | English Region | Latin deriving (Show, Eq, Ord)

data Region = UnitedStates | GreatBritain | Global deriving (Show, Eq, Ord)

-- | IETF language tag.
newtype LanguageTag = LanguageTag{unLanguageTag :: Text} deriving (Eq, Hashable, IsString, SafeCopy)

instance Show LanguageTag where
    show = show . unLanguageTag

-- | Known languages and their tags.
languages :: Map Language LanguageTag
languages = Map.fromList
    [ (Lojban               , "jbo"  )
    , (English Global       , "en"   )
    , (English UnitedStates , "en-US")
    , (English GreatBritain , "en-GB")
    , (Latin                , "la"   )
    ]

-- | The inverse of 'languages'.
languageTags :: HashMap LanguageTag Language
languageTags = HashMap.fromList [ (v,k) | (k,v) <- Map.toList languages ]


-- * Revision Histories
-- ***************************************************************************

type History a = [Revision a]

data Revision a = Revision a User deriving (Eq, Show, Ord, Typeable)


-- * Dictionary
-- ***************************************************************************

type AffixForms = Set Affix

-- | Short affix form associated with a particle or root.
newtype Affix = Affix{unAffix :: Text} deriving (Eq, Ord, IsString, SafeCopy)

instance Show Affix where show = show . unAffix

-- | The grammatical class a particle belongs to, unless it is experimental.
data ParticleClass = GrammaticalClass Text
                   | ExperimentalParticle
                   deriving (Eq, Show, Ord)

instance IsString ParticleClass where
    fromString = GrammaticalClass . fromString

data RootWordStatus = OfficialRoot | ExperimentalRoot deriving (Eq, Show, Ord)

-- | The morphology of a word and associated data specific to each type.
data WordType = ParticleWord AffixForms ParticleClass
              | RootWord AffixForms RootWordStatus
              | CompoundWord
              | LoanWord
              | NameWord
              | ParticleCluster
              deriving (Eq, Show, Ord, Typeable)

-- | Revisable definitions in multiple languages.
type WordTranslations = Map Language (History WordDefinition)

-- | The definition of a word and the optional notes.
data WordDefinition = WordDefinition Text (Maybe Text)
                    deriving (Eq, Show, Ord, Typeable)

instance IsString WordDefinition where
    fromString s = WordDefinition (fromString s) Nothing

data SearchWord = DefinitionWord Text
                | NotesWord Text
                | DefinitionStem Text
                | NotesStem Text
                deriving (Eq, Ord, Typeable)

data KeyWord = KeyWord Language SearchWord deriving (Eq, Ord, Typeable)

newtype Word = Word{unWord :: Text} deriving (Eq, Ord, Typeable, IsString, SafeCopy, ShowQ)

instance Show Word where show = show . unWord

data WordData = WordData
    { word'           :: Word
    , wordType'       :: History WordType
    , wordDefinition' :: WordTranslations
    } deriving (Eq, Ord, Typeable)

instance Show WordData where
    show (WordData w wt wd) = [qq|WordData $w $wt ($wd)|]

instance Indexable WordData where
    empty = ixSet [ ixFun $ \WordData{..} -> [word']
                  , ixFun keyWord
                  ]
      where
        keyWord WordData{wordDefinition' = wd} =
            [ KeyWord language word
              | (language,Revision (WordDefinition d n) _:_) <- Map.toList wd
              , word <-
                  map DefinitionWord (splitWords language d)
               ++ map NotesWord (F.concat $ fmap (splitWords language) n)
            ]

        splitWords lang txt =
#if WINDOWS
            Text.words txt
#else
            case Map.lookup lang languages of
              Nothing -> Text.words txt
              Just (LanguageTag tag) ->
                [ brkBreak br
                  | br <- breaks (breakWord $ Locale $ Text.unpack tag) txt
                  , brkStatus br /= ICU.Uncategorized
                ]
#endif


-- * Lenses
-- ***************************************************************************

makeLenses ''WordData


-- * SafeCopy derivations
-- ***************************************************************************

deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''Region
deriveSafeCopy 0 'base ''Language
deriveSafeCopy 0 'base ''Revision
deriveSafeCopy 0 'base ''ParticleClass
deriveSafeCopy 0 'base ''RootWordStatus
deriveSafeCopy 0 'base ''WordType
deriveSafeCopy 0 'base ''WordDefinition
deriveSafeCopy 0 'base ''SearchWord
deriveSafeCopy 0 'base ''KeyWord
deriveSafeCopy 0 'base ''WordData
