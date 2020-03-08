module KLisp.Parser

where

import KPrelude hiding (try)

import KMonad

import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char

import RIO.List (sortBy)
import RIO.Partial (read, fromJust)


import qualified Data.MultiMap as Q
import qualified RIO.HashMap as M
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
symbol = void . L.symbol sc

-- | Consume all chars until a space is encounterd
word :: Parser Text
word = T.pack <$> some (satisfy wordChar)
  where
    wordChar c = not (isSpace c || c `elem` forbidden)
    forbidden = "()\"\\'" :: String

-- | Run the parser IFF it is followed by a space or eof.
terminated :: Parser a -> Parser a
terminated p = try $ p <* lookAhead (void spaceChar <|> eof)

-- | Run the parser IFF it is not followed by a space or eof.
prefix :: Parser a -> Parser a
prefix p = try $ p <* notFollowedBy (void spaceChar <|> eof)

-- | Create a parser that matches symbols to values and only consumes on match.
fromNamed :: [(Text, a)] -> Parser a
fromNamed = choice . map mkOne . srt
  where
    -- | Sort descending by length of key and then alphabetically
    srt :: [(Text, b)] -> [(Text, b)]
    srt = sortBy . flip on fst $ \a b ->
      case compare (T.length b) (T.length a) of
        EQ -> compare a b
        x  -> x

    -- | Make a parser that matches a terminated symbol or fails
    mkOne (s, x) = terminated (string s) *> pure x

-- | Run a parser between 2 sets of parentheses
paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

-- | Run a parser between 2 sets of parentheses starting with a symbol
statement :: Text -> Parser a -> Parser a
statement s = paren . (symbol s *>)



--------------------------------------------------------------------------------
-- $refs

type Aliases = M.HashMap Text KButton



--------------------------------------------------------------------------------
-- $elem
--
-- Parsers for elements that are not stand-alone KExpr's

-- | Parse a keycode
keycodeP :: Parser Keycode
keycodeP = fromNamed (Q.reverse keyNames ^.. Q.itemed) <?> "keycode"

-- | Parse an integer
numP :: Parser Int
numP = L.decimal

-- | Parse text with escaped characters between "s
textP :: Parser Text
textP = do
  _ <- char '\"'
  s <- manyTill L.charLiteral (char '\"')
  pure . T.pack $ s

-- | Parse a variable reference
derefP :: Parser Text
derefP = prefix (char '@') *> word

--------------------------------------------------------------------------------
-- $tkn

data KExpr
  = KDefIO    DefIO
  | KDefSrc   DefSrc
  | KDefLayer DefLayer
  | KDefAlias Aliases
  deriving Show

--------------------------------------------------------------------------------
-- $cmb
--
-- Parsers built up from the basic KExpr's

-- | Consume an entire file of expressions and comments
configP :: Parser [KExpr]
configP = sc *> exprsP

-- | Parse 0 or more KExpr's
exprsP :: Parser [KExpr]
exprsP = lexeme . many $ lexeme exprP

-- | Parse 1 KExpr
exprP :: Parser KExpr
exprP = paren . choice $
  [ try (symbol "defio")    *> (KDefIO    <$> defioP)
  , try (symbol "defsrc")   *> (KDefSrc   <$> defsrcP)
  , try (symbol "deflayer") *> (KDefLayer <$> deflayerP)
  , try (symbol "defalias") *> (KDefAlias <$> defaliasP)
  ]

--------------------------------------------------------------------------------
-- $but
--
-- All the various ways to refer to buttons


data KButton
  = KRef Text                            -- ^ Reference a named button
  | KEmit Keycode                        -- ^ Emit a keycode
  | KLayerToggle Text                    -- ^ Toggle to a layer when held
  | KTapNext KButton KButton             -- ^ Do 2 things based on behavior
  | KTapHold Int KButton KButton         -- ^ Do 2 things based on behavior and delay
  | KMultiTap [(Int, KButton)] KButton   -- ^ Do things depending on tap-count
  | KAround KButton KButton              -- ^ Wrap 1 button around another
  | KTrans                               -- ^ Transparent button that does nothing
  | KBlock                               -- ^ Button that catches event and does nothing
  deriving Show

-- | Names for various buttons
buttonNames :: [(Text, KButton)]
buttonNames = caps <> syms <> escp <> util
  where
    emitS c = KAround (KEmit KeyLeftShift) (KEmit c)
    pair t c = (T.singleton t, emitS . fromJust . kcFromChar $ c)
    -- Capitalized letters signify shifted letter
    caps = map (\c -> pair c c ) ['A'..'Z']
    -- Certain symbols signify shifted numbers
    syms = map (\(t, c) -> pair t c) $ zip "!@#$%^&*""12345678"
    -- Escaped versions for reserved characters
    escp = [("\\(", emitS Key9), ("\\)", emitS Key0), ("\\_", emitS KeyMinus)]
    -- Extra names for useful buttons
    util = [("_", KTrans), ("_X", KBlock)]

buttonP :: Parser KButton
buttonP = (lexeme . choice . map try $
  [ statement "around"       $ KAround      <$> buttonP <*> buttonP
  , statement "multi-tap"    $ KMultiTap    <$> timed   <*> buttonP
  , statement "tap-hold"     $ KTapHold     <$> numP    <*> buttonP <*> buttonP
  , statement "tap-next"     $ KTapNext     <$> buttonP <*> buttonP
  , statement "layer-toggle" $ KLayerToggle <$> word
  , KRef  <$> derefP
  , lexeme $ fromNamed buttonNames
  , KEmit <$> keycodeP
  ]) <?> "button"

  where
    timed = many ((,) <$> lexeme numP <*> lexeme buttonP)



--------------------------------------------------------------------------------
-- $defio

-- | All different input-tokens KMonad can take
data IToken
  = KDeviceSource Text
  deriving Show

-- | Parse an input token
itokenP :: Parser IToken
itokenP = statement "device-file" $ KDeviceSource <$> textP

-- | All different output-tokens KMonad can take
data OToken
  = KUinputSink Text (Maybe Text)
  deriving Show

-- | Parse an output token
otokenP :: Parser OToken
otokenP = statement "uinput-sink" $ KUinputSink <$> textP <*> optional textP

-- | A collection of all the IO configuration for KMonad
data DefIO = DefIO
  { _itoken  :: IToken     -- ^ How to read key events from the OS
  , _otoken  :: OToken     -- ^ How to write key events to the OS
  , _initStr :: Maybe Text -- ^ Shell command to execute before starting
  }
  deriving Show

-- | Parse the DefIO token
defioP :: Parser DefIO
defioP = do
  it <- lexeme (symbol "input" *> itokenP) -- <?> "Valid input token"
  ot <- lexeme (symbol "output" *> otokenP) -- <?> "Valid output token"
  is <- lexeme $ optional (symbol "init" *> textP)
  pure $ DefIO it ot is

--------------------------------------------------------------------------------
-- $defalias

-- | Parse a collection of names and buttons
defaliasP :: Parser Aliases
defaliasP = M.fromList <$> (many $ (,) <$> lexeme word <*> buttonP)

--------------------------------------------------------------------------------
-- $defsrc

-- | The source layer describes the configuration of the input signal
type DefSrc = [Keycode]

defsrcP :: Parser DefSrc
defsrcP = many $ lexeme keycodeP


--------------------------------------------------------------------------------
-- $deflayer

-- | A layer of buttons
data DefLayer = DefLayer
  { _layerName :: Text
  , _buttons   :: [KButton]
  }
  deriving Show
deflayerP :: Parser DefLayer
deflayerP = DefLayer <$> lexeme word <*> many (lexeme buttonP)


--------------------------------------------------------------------------------
-- $tst

testText :: IO Text
testText = readFileUtf8 "/home/david/prj/hask/kmonad/doc/test.kbd"

test2Text :: IO Text
test2Text = readFileUtf8  "/home/david/prj/hask/kmonad/doc/test2.kbd"

test :: IO ()
test = parseTest configP =<< testText

test2 :: IO ()
test2 = parseTest (some $ lexeme keycodeP) =<< test2Text
