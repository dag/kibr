{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}

-- | Pretty-printing.
module Kibr.Print
    ( -- * Console
      PrettyPrint(..)
    , ppWord
    , ppWords
    )
  where

import qualified Data.Text as Text
import qualified Data.Set  as Set

import Data.Attoparsec.Text         (parseOnly)
import Kibr.Data
import Kibr.Parse                   (Inline(..), Expr(..), tex)
import Text.PrettyPrint.ANSI.Leijen

-- | Format a textual representation of values for printing on the console.
class PrettyPrint a where
    pp :: a -> Doc

instance PrettyPrint Text.Text where
    pp = fillSep . map text . words . Text.unpack

instance PrettyPrint Affix where
    pp = pp . unAffix

instance PrettyPrint Word where
    pp = pp . unWord

instance PrettyPrint ParticleClass where
    pp (GrammaticalClass cls) = pp cls
    pp ExperimentalParticle   = empty

instance PrettyPrint AffixForms where
    pp affixForms
      | Set.null affixForms = empty
      | otherwise           = " -" <> hcat (punctuate "-" (map pp $ Set.toList affixForms)) <> "-"

instance PrettyPrint WordType where
    pp (ParticleWord affixForms ExperimentalParticle) = dullyellow "particle" <+> "(experimental)" <> pp affixForms
    pp (ParticleWord affixForms cls)                  = dullyellow "particle" <> pp affixForms <+>  (bold . green . brackets $ pp cls)
    pp (RootWord affixForms ExperimentalRoot)         = dullyellow "root word" <+> "(experimental)" <> pp affixForms
    pp (RootWord affixForms _)                        = dullyellow "root word" <> pp affixForms
    pp CompoundWord                                   = dullyellow "compound"
    pp LoanWord                                       = dullyellow "loan"
    pp NameWord                                       = dullyellow "name"
    pp ParticleCluster                                = dullyellow "cluster"

instance PrettyPrint WordDefinition where
    pp (WordDefinition def Nothing)      = ppDef def
    pp (WordDefinition def (Just notes)) = ppDef def <> linebreak <> linebreak <> pp notes

instance PrettyPrint Inline where
    pp (Str txt) = pp txt
    pp (Emph txt) = underline $ pp txt
    pp (Bold txt) = bold $ pp txt
    pp (Expr expr) = pp expr

instance PrettyPrint Expr where
    pp (Lit txt) = pp txt
    pp (Sub e1 (Lit e2)) = pp e1 <> pp (Text.map toSub e2)
    pp (Sub e1 e2) = pp e1 <> pp e2
    pp (Sup e1 e2) = pp e1 <> pp e2
    pp (Eql es) = hcat $ punctuate "=" (map pp es)

toSub :: Char -> Char
toSub '0' = '₀'
toSub '1' = '₁'
toSub '2' = '₂'
toSub '3' = '₃'
toSub '4' = '₄'
toSub '5' = '₅'
toSub '6' = '₆'
toSub '7' = '₇'
toSub '8' = '₈'
toSub '9' = '₉'
toSub c   = c

ppDef :: Text.Text -> Doc
ppDef def =
    case parseOnly tex def of
      Left _ -> pp def
      Right is -> fillSep $ map pp is

-- | Format a complete set of data for a 'Word'.
ppWord :: Word -> WordType -> WordDefinition -> Doc
ppWord word typ def = bold (pp word) <+> pp typ <$$> indent 4 (pp def)

ppWords :: Set.Set Word -> Doc
ppWords ws = vsep $ map (dquotes . pp) $ Set.toList ws
