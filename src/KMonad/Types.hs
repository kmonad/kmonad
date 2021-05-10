module KMonad.Types
  ( -- * Parsing
    Parser
  , ParseError
  , Parse(..)

    -- * Defaults
  , Default(..)

  )
where

import KMonad.Prelude

import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- $parsing
--
-- The basic types of parsing

-- | Parser's operate on Text and carry no state
type Parser = Parsec Void Text

-- | The type of errors returned by the Megaparsec parsers
newtype ParseError = ParseError (ParseErrorBundle Text Void)

-- | Use the errorBundlePretty to display ParseError
instance Show ParseError where
  show (ParseError e) = "Parse error at " <> errorBundlePretty e
instance Exception ParseError

-- | Instances of this class have a defined parser
class Parse a where parser :: Parser a

instance Parse Char where parser = anySingle
instance Parse Int  where parser = L.decimal

--------------------------------------------------------------------------------
-- $default
--
-- Class for things that have default values

class Default a where def :: a
