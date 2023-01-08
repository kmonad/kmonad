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
module KMonad.Args.Parser
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

import KMonad.Parsing
import KMonad.Args.Types
import KMonad.Keyboard
import KMonad.Keyboard.ComposeSeq



import Data.Char
import RIO.List (sortBy, find)


import qualified KMonad.Util.MultiMap as Q
import qualified RIO.Text as T
import qualified Text.Megaparsec.Char.Lexer as L


--------------------------------------------------------------------------------
-- $run

-- | Try to parse a list of 'KExpr' from 'Text'
parseTokens :: Text -> Either ParseError [KExpr]
parseTokens t = case runParser configP "" t  of
  Left  e -> Left $ ParseError e
  Right x -> Right x

-- | Load a set of tokens from file, throw an error on parse-fail
loadTokens :: FilePath -> RIO e [KExpr]
loadTokens pth = (readFileUtf8 pth <&> parseTokens) >>= \case
  Left e   -> throwM e
  Right xs -> pure xs


--------------------------------------------------------------------------------
-- $basic

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
    mkOne (s, x) = terminated (string s) $> x

-- | Run a parser between 2 sets of parentheses
paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

-- | Run a parser between 2 sets of parentheses starting with a symbol
statement :: Text -> Parser a -> Parser a
statement s = paren . (symbol s *>)

-- | Run a parser that parser a bool value
bool :: Parser Bool
bool = (symbol "true"  $> True)
   <|> (symbol "false" $> False)

-- | Parse a LISP-like keyword of the form @:keyword value@
keywordP :: Text -> Parser p -> Parser p
keywordP kw p = symbol (":" <> kw) *> lexeme p
  <?> "Keyword " <> ":" <> T.unpack kw

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

-- | Parse text with escaped characters between double quotes.
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
  [ try (symbol "defcfg")   *> (KDefCfg   <$> defcfgP)
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
                 map f $ cps <> num <> oth <> lng
  where
    cps = zip (map T.singleton ['A'..'Z'])
          [ KeyA, KeyB, KeyC, KeyD, KeyE, KeyF, KeyG, KeyH, KeyI, KeyJ, KeyK, KeyL, KeyM,
            KeyN, KeyO, KeyP, KeyQ, KeyR, KeyS, KeyT, KeyU, KeyV, KeyW, KeyX, KeyY, KeyZ ]
    num = zip (map T.singleton "!@#$%^&*")
          [ Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8 ]
    oth = zip (map T.singleton "<>:~\"|{}+?")
          [ KeyComma, KeyDot, KeySemicolon, KeyGrave, KeyApostrophe, KeyBackslash
          , KeyLeftBrace, KeyRightBrace, KeyEqual, KeySlash]
    lng = [ ("quot", KeyApostrophe), ("pipe", KeyBackslash), ("cln", KeySemicolon)
          , ("tild", KeyGrave) , ("udrs", KeyMinus)]

-- | Names for various buttons
buttonNames :: [(Text, DefButton)]
buttonNames = shiftedNames <> escp <> util
  where
    emitS c = KAround (KEmit KeyLeftShift) (KEmit c)
    -- Escaped versions for reserved characters
    escp = [ ("\\(", emitS Key9), ("\\)", emitS Key0)
           , ("\\_", emitS KeyMinus), ("\\\\", KEmit KeyBackslash)]
    -- Extra names for useful buttons
    util = [ ("_", KTrans), ("XX", KBlock)
           , ("lprn", emitS Key9), ("rprn", emitS Key0)]



-- | Parse "X-b" style modded-sequences
moddedP :: Parser DefButton
moddedP = KAround <$> prfx <*> buttonP
  where mods = [ ("S-", KeyLeftShift), ("C-", KeyLeftCtrl)
               , ("A-", KeyLeftAlt),   ("M-", KeyLeftMeta)
               , ("RS-", KeyRightShift), ("RC-", KeyRightCtrl)
               , ("RA-", KeyRightAlt),   ("RM-", KeyRightMeta)]
        prfx = choice $ map (\(t, p) -> prefix (string t) $> KEmit p) mods

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
  s <- case find (\(_, c', _) -> c' == c) ssComposed of
         Nothing -> fail "Unrecognized compose-char"
         Just b  -> pure $ b^._1

  -- If matching, parse a button-sequence from the stored text
  case runParser (some buttonP) "" s of
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
  , ("press-only"     , KPressOnly   <$> keycodeP)
  , ("release-only"   , KReleaseOnly <$> keycodeP)
  , ("multi-tap"      , KMultiTap    <$> timed       <*> buttonP)
  , ("tap-hold"       , KTapHold     <$> lexeme numP <*> buttonP <*> buttonP)
  , ("tap-hold-next"
    , KTapHoldNext <$> lexeme numP <*> buttonP <*> buttonP
                   <*> optional (keywordP "timeout-button" buttonP))
  , ("tap-next-release"
    , KTapNextRelease <$> buttonP <*> buttonP)
  , ("tap-hold-next-release"
    , KTapHoldNextRelease <$> lexeme numP <*> buttonP <*> buttonP
                          <*> optional (keywordP "timeout-button" buttonP))
  , ("tap-next-press"
    , KTapNextPress <$> buttonP <*> buttonP)
  , ("tap-next"       , KTapNext     <$> buttonP     <*> buttonP)
  , ("layer-toggle"   , KLayerToggle <$> lexeme word)
  , ("momentary-layer" , KLayerToggle <$> lexeme word)
  , ("layer-switch"    , KLayerSwitch <$> lexeme word)
  , ("permanent-layer" , KLayerSwitch <$> lexeme word)
  , ("layer-add"      , KLayerAdd    <$> lexeme word)
  , ("layer-rem"      , KLayerRem    <$> lexeme word)
  , ("layer-delay"    , KLayerDelay  <$> lexeme numP <*> lexeme word)
  , ("layer-next"     , KLayerNext   <$> lexeme word)
  , ("around-next"    , KAroundNext  <$> buttonP)
  , ("before-after-next", KBeforeAfterNext <$> buttonP <*> buttonP)
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
  , ("iokit-name"    , KIOKitSource <$> optional textP)]

-- | Parse an output token
otokenP :: Parser OToken
otokenP = choice $ map (try . uncurry statement) otokens

-- | Output tokens to parse; the format is @(keyword, how to parse the token)@
otokens :: [(Text, Parser OToken)]
otokens =
  [ ("uinput-sink"    , KUinputSink <$> lexeme textP <*> optional textP)
  , ("send-event-sink", KSendEventSink <$> optional ((,) <$> lexeme numP <*> numP))
  , ("kext"           , pure KKextSink)]

-- | Parse the DefCfg token
defcfgP :: Parser DefSettings
defcfgP = some (lexeme settingP)

-- | All possible configuration options that can be passed in the defcfg block
settingP :: Parser DefSetting
settingP = let f s p = symbol s *> p in
  (lexeme . choice . map try $
    [ SIToken      <$> f "input"         itokenP
    , SOToken      <$> f "output"        otokenP
    , SCmpSeq      <$> f "cmp-seq"       buttonP
    , SInitStr     <$> f "init"          textP
    , SFallThrough <$> f "fallthrough"   bool
    , SAllowCmd    <$> f "allow-cmd"     bool
    , SCmpSeqDelay <$> f "cmp-seq-delay" numP
    ])

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
