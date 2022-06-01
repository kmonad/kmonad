-- | A collection of general parsing definitions

module KMonad.Parsing
  ( Parser
  , ParserT
  , ParseError(..)

  , sc
  , hsc
  , lex
  , hlex

  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  )

where

import KMonad.Prelude

import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as X


--------------------------------------------------------------------------------

-- | Parsec type specified down to Void Text
type Parser a    = Parsec Void Text a
type ParserT m a = ParsecT Void Text m a

-- | Parsec parse errors under Void Text with an Exception instance
newtype ParseError = ParseError { _parseError :: ParseErrorBundle Text Void}
  deriving Eq

instance Show ParseError where
  show (ParseError e) = "Parse error at " <> errorBundlePretty e

instance Exception ParseError

--------------------------------------------------------------------------------

-- | Horizontal space consumption
hsc :: Parser ()
hsc = X.space space1 empty empty

-- | Horizontal space lexeme
hlex :: Parser a -> Parser a
hlex = X.lexeme hsc

-- | Full space consumption
sc :: Parser ()
sc = X.space space1 (X.skipLineComment  ";;") (X.skipBlockComment "#|" "|#")

-- | Full space lexeme
lex :: Parser a -> Parser a
lex = X.lexeme sc
