{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}

-- | Text manipulation.
module Kibr.Text
    ( -- * Console pretty-printing
      PrettyPrint(..)
    , ppWord
    )
  where

import qualified Data.Text as Text
import qualified Data.Set  as Set

import Kibr.Data
import Text.PrettyPrint.ANSI.Leijen

-- | Format a textual representation of values for printing on the console.
class PrettyPrint a where
    pp :: a -> Doc

instance PrettyPrint Text.Text where
    pp = fillSep . map text . words . Text.unpack

instance PrettyPrint Affix where
    pp (Affix a) = pp a

instance PrettyPrint Word where
    pp (Word w) = pp w

instance PrettyPrint ParticleClass where
    pp (GrammaticalClass cls) = pp cls
    pp ExperimentalParticle   = empty

instance PrettyPrint AffixForms where
    pp affixForms
      | Set.null affixForms = empty
      | otherwise           = " -" <> hcat (punctuate "-" (map pp $ Set.toList affixForms)) <> "-"

instance PrettyPrint WordType where
    pp (ParticleWord affixForms ExperimentalParticle) = underline "particle" <+> "(experimental)" <> pp affixForms
    pp (ParticleWord affixForms cls)                  = underline "particle" <> pp affixForms <+>  (bold . green . brackets $ pp cls)
    pp (RootWord affixForms ExperimentalRoot)         = underline "root word" <+> "(experimental)" <> pp affixForms
    pp (RootWord affixForms _)                        = underline "root word" <> pp affixForms
    pp CompoundWord                                   = underline "compound"
    pp LoanWord                                       = underline "loan"
    pp NameWord                                       = underline "name"
    pp ParticleCluster                                = underline "cluster"

instance PrettyPrint WordDefinition where
    pp (WordDefinition def Nothing)      = pp def
    pp (WordDefinition def (Just notes)) = pp def <> linebreak <> linebreak <> pp notes

-- | Format a complete set of data for a 'Word'.
ppWord :: Word -> WordType -> WordDefinition -> Doc
ppWord word typ def = bold (pp word) <+> pp typ <$$> indent 4 (pp def)
