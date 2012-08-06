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
    )
  where

import Control.Applicative
import Data.Char
import Data.Text (Text)
import Data.Attoparsec.Text as P

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
str = Str <$> P.takeWhile1 (\chr -> chr /= '\\' && chr /= '$') <?> "str"

emph :: Parser Inline
emph = Emph <$> ("\\emph{" .*> P.takeWhile (/= '}') <* P.take 1) <?> "emph"

bold :: Parser Inline
bold = Bold <$> ("\\textbf{" .*> P.takeWhile (/= '}') <* P.take 1) <?> "bold"

expr :: Parser Inline
expr = Expr <$> (char '$' *> (eql <|> sub <|> sup) <* char '$') <?> "expr"

lit :: Parser Expr
lit =
    Lit <$> (open *> alnum <* close <|> alnum) <?> "lit"
  where
    alnum = takeWhile1 isAlphaNum
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
