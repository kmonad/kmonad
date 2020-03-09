{-|

-- | All different input-tokens KMonad can take
data IToken
  = KDeviceSource FilePath
  deriving Show

-- | All different output-tokens KMonad can take
data OToken
  = KUinputSink Text (Maybe Text)
  deriving Show

Module      : KLisp.Parser
Description : The code that turns text into tokens
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

The first stage of reading a config-file: reading in text and turning it into
KExpr-tokens.

-}
module KLisp.Parser
  ( parseTokens
  , loadTokens
  )
where

import KPrelude hiding (try)

import KMonad.Keyboard
import KLisp.Types

import Data.Char
import RIO.List (sortBy)


import qualified Data.MultiMap as Q
import qualified RIO.Text as T
import qualified Text.Megaparsec.Char.Lexer as L


--------------------------------------------------------------------------------
-- $run

parseTokens :: Text -> Either PErrors [KExpr]
parseTokens = runParser configP ""

-- | Load a set of tokens from file, throw an error on parse-fail
loadTokens :: FilePath -> RIO e [KExpr]
loadTokens pth = parseTokens <$> readFileUtf8 pth >>= \case
  Left e   -> throwM e
  Right xs -> pure xs


--------------------------------------------------------------------------------
-- $basic

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

-- | List of all characters that 'end' a word or sequence
terminators :: String
terminators = ")\""

terminatorP :: Parser Char
terminatorP = satisfy (`elem` terminators)

-- | Consume all chars until a space is encounterd
word :: Parser Text
word = T.pack <$> some (satisfy wordChar)
  where wordChar c = not (isSpace c || c `elem` terminators)

-- | Run the parser IFF it is followed by a space, eof, or reserved char
terminated :: Parser a -> Parser a
terminated p = try $ p <* lookAhead (void spaceChar <|> eof <|> void terminatorP)

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
-- $cmb
--
-- Parsers built up from the basic KExpr's

-- | Consume an entire file of expressions and comments
configP :: Parser [KExpr]
configP = sc *> exprsP <* eof

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

-- | Different ways to refer to shifted versions of keycodes
shiftedNames :: [(Text, DefButton)]
shiftedNames = let f = second $ \kc -> KAround (KEmit KeyLeftShift) (KEmit kc) in
                 map f $ cps <> num <> oth
  where
    cps = zip (map T.singleton ['A'..'Z'])
          [ KeyA, KeyB, KeyC, KeyD, KeyE, KeyF, KeyG, KeyH, KeyI, KeyJ, KeyK, KeyL, KeyM,
            KeyN, KeyO, KeyP, KeyQ, KeyR, KeyS, KeyT, KeyU, KeyV, KeyW, KeyX, KeyY, KeyZ ]
    num = zip (map T.singleton "!@#$%^&*")
          [ Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8 ]
    oth = zip (map T.singleton "<>:~\"|{}+")
          [ KeyComma, KeyDot, KeySemicolon, KeyGrave, KeyApostrophe, KeyBackslash
          , KeyLeftBrace, KeyRightBrace, KeyEqual]

-- | Names for various buttons
buttonNames :: [(Text, DefButton)]
buttonNames = shiftedNames <> escp <> util
  where
    emitS c = KAround (KEmit KeyLeftShift) (KEmit c)
    -- Escaped versions for reserved characters
    escp = [ ("\\(", emitS Key9), ("\\)", emitS Key0)
           , ("\\_", emitS KeyMinus), ("\\\\", emitS KeyBackslash)]
    -- Extra names for useful buttons
    util = [("_", KTrans), ("XX", KBlock)]

-- | Parse "X-b" style modded-sequences
moddedP :: Parser DefButton
moddedP = KAround <$> prfx <*> buttonP
  where mods = [ ("S-", KeyLeftShift), ("C-", KeyLeftCtrl)
               , ("A-", KeyLeftAlt),   ("M-", KeyLeftMeta)]
        prfx = choice $ map (\(t, p) -> prefix (string t) *> pure (KEmit p)) mods

-- | 'Reader-macro' style tap-macro
rmTapMacro :: Parser DefButton
rmTapMacro = KTapMacro <$> (char '#' *> paren (some buttonP))


buttonP :: Parser DefButton
buttonP = (lexeme . choice . map try $
  [ statement "around"       $ KAround      <$> buttonP     <*> buttonP
  , statement "multi-tap"    $ KMultiTap    <$> timed       <*> buttonP
  , statement "tap-hold"     $ KTapHold     <$> lexeme numP <*> buttonP <*> buttonP
  , statement "tap-next"     $ KTapNext     <$> buttonP     <*> buttonP
  , statement "layer-toggle" $ KLayerToggle <$> word
  , statement "tap-macro"    $ KTapMacro    <$> some buttonP
  , KRef  <$> derefP
  , lexeme $ fromNamed buttonNames
  , try moddedP
  , lexeme $ try rmTapMacro
  , KEmit <$> keycodeP
  ]) <?> "button"

  where
    timed = many ((,) <$> lexeme numP <*> lexeme buttonP)


--------------------------------------------------------------------------------
-- $defio

-- | Parse an input token
itokenP :: Parser IToken
itokenP = statement "device-file" $ KDeviceSource <$> (T.unpack <$> textP)


-- | Parse an output token
otokenP :: Parser OToken
otokenP = statement "uinput-sink" $ KUinputSink <$> lexeme textP <*> optional textP


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
defaliasP :: Parser DefAlias
defaliasP = many $ (,) <$> lexeme word <*> buttonP

--------------------------------------------------------------------------------
-- $defsrc

defsrcP :: Parser DefSrc
defsrcP = many $ lexeme keycodeP


--------------------------------------------------------------------------------
-- $deflayer
deflayerP :: Parser DefLayer
deflayerP = DefLayer <$> lexeme word <*> many (lexeme buttonP)



--------------------------------------------------------------------------------
-- $tst

-- fname :: String
-- fname = "/home/david/prj/hask/kmonad/doc/example.kbd"

-- testText :: IO Text
-- testText = readFileUtf8 "/home/david/prj/hask/kmonad/doc/test.kbd"

-- test2Text :: IO Text
-- test2Text = readFileUtf8  "/home/david/prj/hask/kmonad/doc/test2.kbd"

-- test3Text :: IO Text
-- test3Text = readFileUtf8  "/home/david/prj/hask/kmonad/doc/example.kbd"

-- test :: IO ()
-- test = parseTest configP =<< testText

-- test2 :: IO ()
-- test2 = parseTest (some $ lexeme keycodeP) =<< test2Text

-- test3 :: IO ()
-- test3 = parseTest configP =<< test3Text
