{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import Data.Char                    (isSpace)
import Data.Maybe                   (fromMaybe)
import Kibr.Data
import Kibr.Parse                   (Inline(..), Expr(..), tex)
import Text.PrettyPrint.ANSI.Leijen

-- | Format a textual representation of values for printing on the console.
class PrettyPrint a where
    pp :: a -> Doc

instance PrettyPrint Text.Text where
    pp t =
        pre <> (fillSep . map text . words . Text.unpack $ t) <> suf
      where
        pre = text $ Text.unpack $ Text.takeWhile isSpace t
        suf = if " " `Text.isSuffixOf` t then " " else ""

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
    pp (WordDefinition def (Just notes)) = ppDef def <> linebreak <> linebreak <> ppDef notes

instance PrettyPrint Inline where
    pp (Str txt)   = pp txt
    pp (Link w)    = underline $ pp w
    pp (Emph txt)  = underline $ pp txt
    pp (Bold txt)  = bold $ pp txt
    pp (Expr expr) = pp expr

instance PrettyPrint Expr where
    pp (Lit txt)         = pp txt
    pp (Sub e1 (Lit e2)) = pp e1 <> pp (Text.map toSub e2)
    pp (Sub e1 e2)       = pp e1 <> pp e2
    pp (Sup e1 (Lit e2)) = pp e1 <> pp (Text.map toSup e2)
    pp (Sup e1 e2)       = pp e1 <> pp e2
    pp (Eql es)          = hcat $ punctuate "=" (map pp es)

toSub :: Char -> Char
toSub c = fromMaybe c $ lookup c $ zip ['0'..'9'] ['₀'..'₉']

toSup :: Char -> Char
toSup c = fromMaybe c $ lookup c $ zip "+-0123456789" "⁺⁻⁰¹²³⁴⁵⁶⁷⁸⁹"

ppDef :: Text.Text -> Doc
ppDef def =
    case parseOnly tex def of
      Left _ -> pp def
      Right is -> fillCat $ map pp is

-- | Format a complete set of data for a 'Word'.
ppWord :: Word -> WordType -> WordDefinition -> Doc
ppWord word typ def = bold (pp word) <+> pp typ <$$> indent 4 (pp def)

ppWords :: Set.Set Word -> Doc
ppWords ws = vsep $ map (dquotes . pp) $ Set.toList ws
