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



type Parser = Parsec Void Text
type PError = ParseErrorBundle Text Void

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment  "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

--------------------------------------------------------------------------------
-- $tkn

-- We take a 2-step approach to parsing, first we decode the raw text into a
-- list of KTokens, which essentially only parse the structure of the code,
-- then we parse the KTokens into a config.

-- parseTokens :: Text -> Either PError [KToken]
-- parseTokens = runParser (many $ lexeme tokenP) ""

tokensP :: Parser [KToken]
tokensP = many $ lexeme tokenP

tokenP :: Parser KToken
tokenP = choice [numP, textP, listP, atomP]

listP :: Parser KToken
listP = fmap KList
      . between (char '(') (char ')')
      $ some (lexeme tokenP)

textP :: Parser KToken
textP = fmap (KText . T.pack) $ char '\"' *> manyTill L.charLiteral (char '\"')

      -- . (char '"') (char '"')
      -- $ many L.charLiteral

numP :: Parser KToken
numP = KNum <$> L.decimal

atomP :: Parser KToken
atomP = do
  h <- satisfy isAlpha
  t <- many $ satisfy (\c -> isAlphaNum c || c `elem` extra)
  pure . KAtom . T.pack $ h:t
  where extra = "-!?_*" :: String
