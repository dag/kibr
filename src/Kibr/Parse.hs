{-# LANGUAGE OverloadedStrings #-}

-- | Text parsing.
module Kibr.Parse
    ( -- * TeX
      TeX
    , Inline(..)
    , Expr(..)
    , tex
    , str
    , emph
    , bold
    , expr
    , lit
    , sub
    , sup
    , eql
      -- * Notes with word links
    , Notes
    , NotesPart(..)
    , notes
    , notesText
    , wordLink
    )
  where

import Control.Applicative
import Data.Char
import Data.Text (Text)
import Data.Attoparsec.Text as P
import Kibr.Data

type TeX = [Inline]

data Inline = Str Text   -- ^ A sequence of text.
            | Emph Text  -- ^ Emphasized text.
            | Bold Text  -- ^ Bold text.
            | Expr Expr  -- ^ A mathematical expression.
            deriving (Eq, Show)

data Expr = Lit Text       -- ^ A literal number or variable.
          | Sub Expr Expr  -- ^ Subscript.
          | Sup Expr Expr  -- ^ Superscript.
          | Eql [Expr]     -- ^ A sequence of equations.
          deriving (Eq, Show)

tex :: Parser TeX
tex = many (emph <|> bold <|> expr <|> str) <?> "tex"

str :: Parser Inline
str = Str <$> P.takeWhile1 ((&&) <$> (/= '\\') <*> (/= '$')) <?> "str"

emph :: Parser Inline
emph = Emph <$> ("\\emph{" .*> P.takeWhile (/= '}') <*. "}") <?> "emph"

bold :: Parser Inline
bold = Bold <$> ("\\textbf{" .*> P.takeWhile (/= '}') <*. "}") <?> "bold"

expr :: Parser Inline
expr = Expr <$> (char '$' *> (eql <|> sub <|> sup) <* char '$') <?> "expr"

lit :: Parser Expr
lit =
    Lit <$> (open *> alnum <* close <|> alnum) <?> "lit"
  where
    alnum = takeWhile1 $ (||) <$> isAlphaNum <*> inClass "+-"
    open  = char '{'
    close = char '}'

sub :: Parser Expr
sub = Sub <$> lit <*> (char '_' *> lit) <?> "sub"

sup :: Parser Expr
sup = Sup <$> lit <*> (char '^' *> lit) <?> "sup"

eql :: Parser Expr
eql =
    Eql <$> ((:) <$> e <*> some (char '=' *> e)) <?> "eql"
  where
    e = sub <|> sup <|> lit

type Notes = [NotesPart]

data NotesPart = NotesText Text
               | WordLink Word
               deriving (Eq, Show)

notes :: Parser Notes
notes = many (wordLink <|> notesText) <?> "notes"

notesText :: Parser NotesPart
notesText = NotesText <$> P.takeWhile1 (/= '{') <?> "notesText"

wordLink :: Parser NotesPart
wordLink = (WordLink . Word) <$> ("{" .*> P.takeWhile (/= '}') <*. "}") <?> "wordLink"
