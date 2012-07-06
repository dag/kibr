{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, RecordWildCards, TemplateHaskell #-}

-- | Application data model.
module Kibr.Data
    ( -- * Users
      User(..)
      -- * Languages
    , Language(..)
    , Region(..)
    , LanguageTag(..)
    , languages
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
    , Word(..)
    , WordData(WordData)
    , word
    , wordType
    , wordDefinition
      -- * Utilities
    , IsList(fromList)
    )
  where

import qualified Data.ByteString as Bytes
import qualified Data.IxSet      as IxSet
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.Text       as Text

import Data.ByteString               (ByteString)
import Data.Default                  (Default(def))
import Data.IxSet                    (IxSet, Indexable, ixSet, ixFun)
import Data.Map                      (Map)
import Data.SafeCopy                 (SafeCopy, deriveSafeCopy, base)
import Data.Set                      (Set)
import Data.String                   (IsString(fromString))
import Data.Text                     (Text)
import Data.Typeable                 (Typeable)
import Data.Word                     (Word8)
import Lens.Family2.TH               (mkLenses)
import Text.InterpolatedString.Perl6 (qq)


-- * Utilities
-- ***************************************************************************

-- | Types that lists can be converted into, similar to 'IsString', only
-- there's currently no @OverloadedLists@ extension in GHC and 'fromList'
-- must be called explicitly even for list literals.  The usefulness of
-- this class is primarily to reduce the need for qualified imports of
-- modules exporting @fromList@ functions such as those in /containers/ and
-- /ixset/.
class IsList t a | t -> a where
    fromList :: [a] -> t

instance IsList [a] a where
    fromList = id

instance (Ord a) => IsList (Set a) a where
    fromList = Set.fromList

instance (Ord k) => IsList (Map k a) (k, a) where
    fromList = Map.fromList

instance (Indexable a, Ord a, Typeable a) => IsList (IxSet a) a where
    fromList = IxSet.fromList

instance IsList ByteString Word8 where
    fromList = Bytes.pack


-- * Users
-- ***************************************************************************

data User = SystemUser deriving (Eq, Show, Ord)


-- * Languages
-- ***************************************************************************

data Language = Lojban | English Region deriving (Show, Eq, Ord)

data Region = UnitedStates | GreatBritain | Global deriving (Show, Eq, Ord)

-- | IETF language tag.
newtype LanguageTag = LanguageTag Text deriving (Eq, IsString, SafeCopy)

instance Show LanguageTag where
    show (LanguageTag t) = show t

-- | Known languages and their tags.
languages :: Map Language LanguageTag
languages = fromList
    [ (Lojban               , "jbo"  )
    , (English Global       , "en"   )
    , (English UnitedStates , "en-US")
    , (English GreatBritain , "en-GB")
    ]


-- * Revision Histories
-- ***************************************************************************

type History a = [Revision a]

data Revision a = Revision a User deriving (Eq, Show, Ord, Typeable)


-- * Dictionary
-- ***************************************************************************

type AffixForms = Set Affix

-- | Short affix form associated with a particle or root.
newtype Affix = Affix Text deriving (Eq, Ord, IsString, SafeCopy)

instance Show Affix where show (Affix a) = show a

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

newtype Word = Word Text deriving (Eq, Ord, Typeable, IsString, SafeCopy)

instance Show Word where show (Word w) = show w

data WordData = WordData
    { word            :: Word
    , _wordType       :: History WordType
    , _wordDefinition :: WordTranslations
    } deriving (Eq, Ord, Typeable)

mkLenses ''WordData

instance Show WordData where
    show (WordData w wt wd) = [qq|WordData $w $wt ($wd)|]

instance Indexable WordData where
    empty = ixSet [ ixFun $ \WordData{..} -> [word] ]


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
deriveSafeCopy 0 'base ''WordData
