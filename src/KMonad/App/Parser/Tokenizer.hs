{-|
Module      : KMonad.Args.Parser
Description : How to turn a text-file into config-tokens
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

We perform configuration parsing in 2 steps:
- 1. We turn the text-file into a token representation
- 2. We check the tokens and turn them into an AppCfg

This module covers step 1.

-}
module KMonad.App.Parser.Tokenizer
  ( -- * Parsing 'KExpr's
    parseTokens
  , loadTokens

  -- * Building Parsers
  , symbol
  , numP

  -- * Parsers for Tokens and Buttons
  , otokens
  , itokens
  , keywordButtons
  , noKeywordButtons
  )
where

import KMonad.Prelude hiding (try, bool)

import KMonad.App.Parser.Keycode
import KMonad.App.Parser.Operations
import KMonad.App.Parser.Types
import KMonad.App.KeyIO
import KMonad.Util.Keyboard

import Data.Char
import RIO.List (sortBy, find)


import qualified RIO.HashMap as M
import qualified RIO.Text as T
import qualified Text.Megaparsec.Char.Lexer as L


--------------------------------------------------------------------------------
-- $run

-- | Try to parse a list of 'KExpr' from 'Text'
parseTokens :: Text -> Either PErrors [KExpr]
parseTokens t = case runParser configP "" t  of
  Left  e -> Left $ PErrors e
  Right x -> Right x

-- | Load a set of tokens from file, throw an error on parse-fail
loadTokens :: (MonadIO m, MonadThrow m) => FilePath -> m [KExpr]
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

-- | List of all characters that /end/ a word or sequence
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

-- | Run a parser that parser a bool value
bool :: Parser Bool
bool = symbol "true" *> pure True
   <|> symbol "false" *> pure False

-- | Parse a LISP-like keyword of the form @:keyword value@
keywordP :: Text -> Parser p -> Parser p
keywordP kw p = lexeme (string (":" <> kw)) *> lexeme p
  <?> "Keyword " <> ":" <> T.unpack kw

--------------------------------------------------------------------------------
-- $elem
--
-- Parsers for elements that are not stand-alone KExpr's

-- | Parse an integer
numP :: Parser Int
numP = L.decimal

-- | Parse text with escaped characters between "s
textP :: Parser Text
textP = do
  _ <- char '\"' <|> char '\''
  s <- manyTill L.charLiteral (char '\"' <|> char '\'')
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
  [ try (symbol "defcfg")   *> (KDefCfg   <$> defcfgP)
  , try (symbol "defsrc")   *> (KDefSrc   <$> defsrcP)
  , try (symbol "deflayer") *> (KDefLayer <$> deflayerP)
  , try (symbol "defalias") *> (KDefAlias <$> defaliasP)
  ]

-- | Parse a (123, 456) tuple as a KeyRepeatCfg
repCfgP :: Parser KeyRepeatCfg
repCfgP = lexeme $ paren $ do
  a <- numP
  _ <- char ','
  b <- numP
  pure $ KeyRepeatCfg (fi a) (fi b)

--------------------------------------------------------------------------------
-- $but
--
-- All the various ways to refer to buttons

-- | Turn 2 strings into a list of singleton-Text tuples by zipping the lists.
--
-- z "abc" "123" -> [("a", "1"), ("b", 2) ...]
z :: String -> String -> [(Text, Text)]
z a b = uncurry zip $ over (both.traversed) T.singleton (a, b)

-- | Different ways to refer to shifted versions of keycodes
shiftedNames :: [(Text, DefButton)]
shiftedNames = map (second (shiftedOf . CoreName)) $ cps <> num <> oth where
  cps = z ['A'..'Z'] ['a'..'z']
  num = z "!@#$%^&*" "12345678"
  oth = z "<>:~\"|{}+?" -- NOTE: \" is an escaped " and lines up with '
          ",.;`'\\[]=/" -- NOTE: \\ is an escaped \ and lines up with |

-- | Create a button that emits some keycode while holding shift
shiftedB :: CoreName -> DefButton
shiftedB = KAround (KEmit $ kc "lsft") . KEmit . kc

-- | Names for various buttons
buttonNames :: [(Text, DefButton)]
buttonNames = shiftedNames <> escp <> util
  where
    -- Escaped versions for reserved characters
    escp = [ ("\\(", shiftedB "9"), ("\\)", shiftedB "0")
           , ("\\_", shiftedB "-"), ("\\\\", KEmit $ kc "\\")]
    -- Extra names for useful buttons
    util = [ ("_", KTrans), ("XX", KBlock)
           , ("lprn", shiftedB "9"), ("rprn", shiftedB "0")]

-- | Parse "X-b" style modded-sequences
moddedP :: Parser DefButton
moddedP = KAround <$> prfx <*> buttonP
  where mods = [ ("S-",  kc "lsft"), ("C-",  kc "lctl")
               , ("A-",  kc "lalt"), ("M-",  kc "lmet")
               , ("RS-", kc "rsft"), ("RC-", kc "rctl")
               , ("RA-", kc "ralt"), ("RM-", kc "rmet")]
        prfx = choice $ map (\(t, p) -> prefix (string t) *> pure (KEmit p)) mods

-- | Parse Pxxx as pauses (useful in macros)
pauseP :: Parser DefButton
pauseP = KPause . fromIntegral <$> (char 'P' *> numP)

-- | #()-syntax tap-macro
rmTapMacroP :: Parser DefButton
rmTapMacroP =
  char '#' *> paren (KTapMacro <$> some buttonP
                               <*> optional (keywordP "delay" numP))

-- | Compose-key sequence
composeSeqP :: Parser [DefButton]
composeSeqP = do
  -- Lookup 1 character in the compose-seq list
  c <- anySingle <?> "special character"
  s <- case find (\(_, c', _) -> (c' == c)) ssComposed of
         Nothing -> fail "Unrecognized compose-char"
         Just b  -> pure $ b^._1

  -- If matching, parse a button-sequence from the stored text
  --
  -- NOTE: Some compose-sequences contain @_@ characters, which would be parsed
  -- as 'Transparent' if we only used 'buttonP', that is why we are prefixing
  -- that parser with one that check specifically and only for @_@ and matches
  -- it to @shifted min@

  let underscore = shiftedB "-" <$ lexeme (char '_')

  case runParser (some $ underscore <|> buttonP) "" s of
    Left  _ -> fail "Could not parse compose sequence"
    Right b -> pure b

-- | Parse a dead-key sequence as a `+` followed by some symbol
deadkeySeqP :: Parser [DefButton]
deadkeySeqP = do
  _ <- prefix (char '+')
  c <- satisfy (`elem` ("~'^`\"," :: String))
  case runParser buttonP "" (T.singleton c) of
    Left  _ -> fail "Could not parse deadkey sequence"
    Right b -> pure [b]

-- | Parse any button
buttonP :: Parser DefButton
buttonP = (lexeme . choice . map try $
  map (uncurry statement) keywordButtons ++ noKeywordButtons
  ) <?> "button"

-- | Parsers for buttons that have a keyword at the start; the format is
-- @(keyword, how to parse the token)@
keywordButtons :: [(Text, Parser DefButton)]
keywordButtons =
  [ ("around"         , KAround      <$> buttonP     <*> buttonP)
  , ("multi-tap"      , KMultiTap    <$> timed       <*> buttonP)
  , ("tap-hold"       , KTapHold     <$> lexeme numP <*> buttonP <*> buttonP)
  , ("tap-hold-next"  , KTapHoldNext <$> lexeme numP <*> buttonP <*> buttonP)
  , ("tap-next-release"
    , KTapNextRelease <$> buttonP <*> buttonP)
  , ("tap-hold-next-release"
    , KTapHoldNextRelease <$> lexeme numP <*> buttonP <*> buttonP)
  , ("tap-next"       , KTapNext     <$> buttonP     <*> buttonP)
  , ("layer-toggle"   , KLayerToggle <$> lexeme word)
  , ("layer-switch"   , KLayerSwitch <$> lexeme word)
  , ("layer-add"      , KLayerAdd    <$> lexeme word)
  , ("layer-rem"      , KLayerRem    <$> lexeme word)
  , ("layer-delay"    , KLayerDelay  <$> lexeme numP <*> lexeme word)
  , ("layer-next"     , KLayerNext   <$> lexeme word)
  , ("around-next"    , KAroundNext  <$> buttonP)
  , ("around-next-timeout", KAroundNextTimeout <$> lexeme numP <*> buttonP <*> buttonP)
  , ("tap-macro"
    , KTapMacro <$> lexeme (some buttonP) <*> optional (keywordP "delay" numP))
  , ("tap-macro-release"
    , KTapMacroRelease <$> lexeme (some buttonP) <*> optional (keywordP "delay" numP))
  , ("cmd-button"     , KCommand     <$> lexeme textP <*> optional (lexeme textP))
  , ("pause"          , KPause . fromIntegral <$> numP)
  , ("sticky-key"     , KStickyKey   <$> lexeme numP <*> buttonP)
  ]
 where
  timed :: Parser [(Int, DefButton)]
  timed = many ((,) <$> lexeme numP <*> lexeme buttonP)

-- | Parsers for buttons that do __not__ have a keyword at the start
noKeywordButtons :: [Parser DefButton]
noKeywordButtons =
  [ KComposeSeq <$> deadkeySeqP
  , KRef  <$> derefP
  , lexeme $ fromNamed buttonNames
  , try moddedP
  , lexeme $ try rmTapMacroP
  , lexeme $ try pauseP
  , KEmit <$> keycodeP
  , KComposeSeq <$> composeSeqP
  ]

--------------------------------------------------------------------------------
-- $defcfg

-- | Parse an input token
itokenP :: Parser IToken
itokenP = choice $ map (try . uncurry statement) itokens

-- | Input tokens to parse; the format is @(keyword, how to parse the token)@
itokens :: [(Text, Parser IToken)]
itokens =
  [ ("device-file"   , KDeviceSource <$> (T.unpack <$> textP))
  , ("low-level-hook", pure KLowLevelHookSource)
  , ("iokit-name"    , KIOKitSource <$> optional (textP >>= mkCfg))]
  where
    mkCfg n = pure $ def {_cfgProductName = n}

-- | Parse an output token
otokenP :: Parser OToken
otokenP = choice $ map (try . uncurry statement) otokens

-- | Output tokens to parse; the format is @(keyword, how to parse the token)@
otokens :: [(Text, Parser OToken)]
otokens =
  [ ("uinput-sink"    , KUinputSink <$> lexeme textP <*> lexeme (optional textP) <*> lexeme (optional repCfgP))
  , ("send-event-sink", KSendEventSink <$> optional (lexeme numP) <*> optional (lexeme numP))
  , ("dext"           , pure KExtSink)
  , ("kext"           , pure KExtSink)]

-- | Parse the DefCfg token
defcfgP :: Parser DefSettings
defcfgP = some (lexeme settingP)

-- | All the settable settings in a `defcfg` block
settings :: [(Text, Parser DefSetting)]
settings =
    [ ("input"         , SIToken      <$> itokenP)
    , ("output"        , SOToken      <$> otokenP)
    , ("cmp-seq-delay" , SCmpSeqDelay <$> numP)
    , ("cmp-seq"       , SCmpSeq      <$> buttonP)
    , ("init"          , SInitStr     <$> textP)
    , ("fallthrough"   , SFallThrough <$> bool)
    , ("allow-cmd"     , SAllowCmd    <$> bool)
    ]

-- | All possible configuration options that can be passed in the defcfg block
settingP :: Parser DefSetting
settingP = lexeme . choice . map (\(s, p) -> (try $ symbol s) *> p) $ settings


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
