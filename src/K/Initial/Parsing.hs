{- | TODO: insert header

The 'K.Initial.Parsing' module adds various parsing tools to the basic Prelude.
This submodule should not be reexported from 'K.Initial', as many modules do not
deal with parsing. Consider this an additional parsing Prelude.
-}
module K.Initial.Parsing
  ( Parser
  , ParserT
  , ParseError(..)
  , AsParseError(..)

  , parse
  , parseT

  , sc
  , hsc
  , lex
  , hlex
  , symbol

  , msP
  , natP
  , textP
  , listOfP

  , terminators
  , terminated
  , maybeRestP
  , namedP
  , pairP
  , prefixP

  , module Data.Char
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  )
where

import K.Initial.Initial
import K.Initial.Util

import Data.Char
import Text.Megaparsec hiding (ParseError, parse, noneOf)
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as X
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified Control.Monad.Error.Lens as Err

-- basic types -----------------------------------------------------------------

type Parser a    = Parsec Void Text a    -- ^ Parser of Text, no custom error
type ParserT m a = ParsecT Void Text m a -- ^ ParserT of Text, no custom error

-- | 'ParseErrorBundle' wrapped in a newtype with an 'Exception' instance
newtype ParseError = ParseError { _parseError :: ParseErrorBundle Text Void}
  deriving Eq
makeClassyPrisms ''ParseError

-- | Show errors as their 'errorBundlePretty' representation
instance Show ParseError where
  show (ParseError e) = "Parse error at " <> errorBundlePretty e

instance Exception ParseError
instance AsParseError SomeException where __ParseError = _SomeException

-- ops -------------------------------------------------------------------------

-- -- | Run a 'Parser' on some 'Text'
parse :: (AsParseError e, MonadError e m) => Parser a -> Text -> m a
parse p t = case runIdentity $ parseT p t of
  Left e -> errThrowing __ParseError e
  Right x -> pure x

-- | Run a 'ParserT' on some 'Text'
parseT :: (AsParseError e, MonadError e m1, Monad m2)
  => ParserT m2 a -> Text -> m2 (m1 a)
parseT p t = runParserT p "" t >>= \case
  Left e -> pure $ errThrowing _ParseError e
  Right x -> pure $ pure x

-- whitespace ------------------------------------------------------------------

-- | Horizontal space consumption
hsc :: ParserT m ()
hsc = X.space space1 empty empty

-- | Horizontal space lexeme
hlex :: ParserT m a -> ParserT m a
hlex = X.lexeme hsc

-- | Full space consumption
sc :: ParserT m ()
sc = X.space space1 (X.skipLineComment  ";;") (X.skipBlockComment "#|" "|#")

-- | Full space lexeme
lex :: ParserT m a -> ParserT m a
lex = X.lexeme sc

-- simple parsers --------------------------------------------------------------

-- | Parse a non-negative integer
natP :: ParserT m Natural
natP = X.decimal <?> "natural number"

-- | Parse a natural number as a 'Dt' expressed as in milliseconds
msP :: ParserT m Dt
msP = view (from ms) <$> natP <?> "natural number expressing ms"

-- | Parse a ""-surrounded string, supporting @\@-style escapes
textP :: ParserT m Text
textP = pack <$> (char '\"' *> manyTill X.charLiteral (char '\"' ))


-- | Return 'Nothing' if at 'eof', otherwise 'Just' the rest of the text
maybeRestP :: ParserT m (Maybe Text)
maybeRestP = Nothing <$ eof <|> Just <$> takeRest

-- | Consume 1 symbol
symbol :: Text -> ParserT m ()
symbol = void . X.symbol sc


-- combinators -----------------------------------------------------------------

-- | Run the parser IFF it is not followed by a space or eof.
prefixP :: ParserT m a -> ParserT m a
prefixP p = try $ p <* notFollowedBy (void spaceChar <|> eof)

-- | List of all characters that /end/ a word or sequence
terminators :: String
terminators = ")\""

terminated :: ParserT m a -> ParserT m a
terminated p = try $ do
  x <- p

  _ <- lookAhead $ eof <|> void eol <|> space1 <|> void (satisfy (`elem` terminators))
  pure x

-- | Parse a []-surrounded string with ,-separated values
listOfP :: ParserT m a -> ParserT m [a]
listOfP p = between (char '[') (char ']') $ sepBy p (hlex $ char ',')

-- | Create a parser that matches symbols to values and only consumes on match.
namedP :: Named a -> ParserT m a
namedP = do
  let mkP (s, x) = terminated (string s) $> x
  choice . map mkP . L.sortBy (bigger `on` fst)

-- | Create a parser that matches 2 parsers in sequence as a tuple
pairP :: ParserT m a -> ParserT m b -> ParserT m (a, b)
pairP a b = (,) <$> a <*> b
