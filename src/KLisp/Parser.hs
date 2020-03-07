module KLisp.Parser

where

import KPrelude
import KLisp.Token

import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified RIO.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- $basic

-- | Parser's operate on Text and carry no state
type Parser = Parsec Void Text

-- | Errors defined to match Parser's
type PError = ParseErrorBundle Text Void

-- | Consume whitespace
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment  ";;")
  (L.skipBlockComment "#|" "|#")

-- | Consume whitespace after the provided parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Consume 1 symbol
symbol :: Text -> Parser ()
symbol = L.symbol sc

--------------------------------------------------------------------------------
-- $tkn



-- | Parse a full config file
configP :: Parser [KToken]
configP = sc *> tokensP <* eof

-- | Parse any number of tokens
tokensP :: Parser [KToken]
tokensP = many $ lexeme tokenP

-- | Parse any token
tokenP :: Parser KToken
tokenP = choice [numP, textP, listP, atomP]

-- | Parse a list of tokens
listP :: Parser KToken
listP = fmap KList
      . between (char '(') (char ')')
      $ some (lexeme tokenP)

-- | Parse a text literal
textP :: Parser KToken
textP = fmap (KText . T.pack) $ char '\"' *> manyTill L.charLiteral (char '\"')

-- | Parse an int literal
numP :: Parser KToken
numP = KNum <$> L.decimal

-- | Parse any number of characters
atomP :: Parser KToken
atomP = do
  cs <- satisfy $ not . (`elem` ("\"") )

  do

  h <- satisfy isAlp
  t <- many $ satisfy (\c -> isAlphaNum c || c `elem` extra)
  pure . KAtom . T.pack $ h:t
  where extra = "-!?_*/" :: String


--------------------------------------------------------------------------------
-- $tst

testText :: IO Text
testText = readFileUtf8 "/home/david/prj/hask/kmonad/doc/example.kbd"

test :: IO ()
test = parseTest configP =<< testText

-- parseTokens :: Text -> Either PError [KToken]
-- parseTokens = parseTest configP
-- parseTokens = runParser configP ""
