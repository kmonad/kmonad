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
  , keyword
  , sexpr
  , numP
  , mkTokenP'

  -- * Parsers for Tokens and Buttons
  , otokens
  , itokens
  , buttonP
  , buttonP'
  , implArndButtons
  , implArndP
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
import RIO.List (find)


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

-- | A predicate to check if a characters /ends/ a word or sequence
isDelimiter :: Char -> Bool
isDelimiter c = elem @[] c "()\"" || isSpace c

terminatorP :: Parser ()
terminatorP = void (satisfy isDelimiter) <|> eof <?> "end of token / delimiter"

-- | Consume all chars until a delimiter is encounterd
word :: Parser Text
word = lexeme . takeWhile1P Nothing $ not . isDelimiter

-- | Run the parser IFF it is terminated
terminated :: Parser a -> Parser a
terminated p = try . lexeme $ p <* lookAhead terminatorP

-- | Keywords are terminated symbols
keyword :: Text -> Parser ()
keyword = terminated . void . string -- `string` intead of `symbol`, since `terminated` already does `lexeme`

keyword' :: Text -> a -> Parser a
keyword' k x = keyword k $> x

-- | Run a parser designed to parse an inner S-expression.
sexpr :: Text -> Parser a -> Parser a
sexpr s p = keyword s *> p

-- | Create a parser that matches symbols to values and only consumes on match.
fromNamed :: [(Text, a)] -> Parser a
fromNamed = choice . map (uncurry keyword')

-- | Run a parser between 2 sets of parentheses
paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

-- | Run a parser that parser a bool value
boolP :: Parser Bool
boolP = keyword' "true" True <|> keyword' "false" False

-- | Parse an option argument of the form @:keyword value@
optargP :: Text -> Parser p -> Parser p
optargP a p = keyword (":" <> a) *> p
  <?> "Keyword " <> ":" <> unpack a

optarg'P :: Text -> Parser p -> Parser (Maybe p)
optarg'P a = optional . optargP a

-- | Parse based on a liste of sexpressions
mkTokenP :: [(Text, Parser a)] -> Parser a
mkTokenP = mkTokenP' False

mkTokenP' :: Bool -> [(Text, Parser a)] -> Parser a
mkTokenP' toplevel tkns = paren' . choice $ uncurry sexpr <$> tkns
 where
  paren' = if toplevel then id else paren

--------------------------------------------------------------------------------
-- $elem
--
-- Parsers for elements that are not stand-alone KExpr's

-- | Parse a keycode
keycodeP :: Parser Keycode
keycodeP = fromNamed (Q.reverse keyNames ^.. Q.itemed) <?> "keycode"

-- | Parse an integer
numP :: Parser Int
numP = terminated L.decimal

-- | Parse text with escaped characters between double quotes.
textP :: Parser Text
textP = lexeme $ char '"' >> pack <$> manyTill L.charLiteral (char '"')

-- | Parse a variable reference
derefP :: Parser Text
derefP = char '@' *> word

--------------------------------------------------------------------------------
-- $cmb
--
-- Parsers built up from the basic KExpr's

-- | Consume an entire file of expressions and comments
configP :: Parser [KExpr]
configP = sc *> exprsP <* eof

-- | Parse 0 or more KExpr's
exprsP :: Parser [KExpr]
exprsP = many exprP

-- | Parse 1 KExpr
exprP :: Parser KExpr
exprP = paren . choice $
  [ sexpr "defcfg"    $ KDefCfg   <$> defcfgP
  , sexpr "defsrc"    $ KDefSrc   <$> defsrcP
  , sexpr "deflayer"  $ KDefLayer <$> deflayerP
  , sexpr "defalias"  $ KDefAlias <$> defaliasP
  ]

--------------------------------------------------------------------------------
-- $but
--
-- All the various ways to refer to buttons

shifted :: Keycode -> DefButton
shifted = KAroundImplicit (KEmit KeyLeftShift) . KEmit

-- | Different ways to refer to shifted versions of keycodes
shiftedNames :: [(Text, DefButton)]
shiftedNames = second shifted <$> cps <> num <> oth <> lng
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
    -- Escaped versions for reserved characters
    escp = [ ("\\(", shifted Key9), ("\\)", shifted Key0)
           , ("\\_", shifted KeyMinus), ("\\\\", KEmit KeyBackslash)]
    -- Extra names for useful buttons
    util = [ ("_", KTrans), ("XX", KBlock)
           , ("lprn", shifted Key9), ("rprn", shifted Key0)]

-- | Parse "X-b" style modded-sequences
moddedP :: Parser DefButton
moddedP = KAroundImplicit <$> prfx <*> buttonP
  where mods = [ ("S-", KeyLeftShift), ("C-", KeyLeftCtrl)
               , ("A-", KeyLeftAlt),   ("M-", KeyLeftMeta)
               , ("RS-", KeyRightShift), ("RC-", KeyRightCtrl)
               , ("RA-", KeyRightAlt),   ("RM-", KeyRightMeta)]
        prfx = choice $ map (\(t, p) -> try $ string t $> KEmit p) mods

-- | Parse Pxxx as pauses (useful in macros)
pauseP :: Parser DefButton
pauseP = KPause . fromIntegral <$> (char 'P' *> numP)

-- | #()-syntax tap-macro
rmTapMacroP :: Parser DefButton
rmTapMacroP =
  try (symbol "#(")
    >> KTapMacro <$> some buttonP <*> optarg'P "delay" numP
    <* symbol ")"

-- | Compose-key sequence
composeSeqP :: Parser [DefButton]
composeSeqP = do
  -- Lookup 1 character in the compose-seq list
  s <- terminated $ do
    c <- anySingle <?> "special character"
    case find (\(_, c', _) -> c' == c) ssComposed of
      Nothing -> fail "Unrecognized compose-char"
      Just b  -> pure $ b^._1

  -- If matching, parse a button-sequence from the stored text
  case runParser (some buttonP <* eof) "" s of
    Left  _ -> fail $ "Internal error: Could not parse compose sequence `" <> unpack s <> "`"
    Right b -> pure b

-- | Parse a dead-key sequence as a `+` followed by some symbol
deadkeySeqP :: Parser [DefButton]
deadkeySeqP = do
  c <- terminated $ char '+' *> satisfy (`elem` ("~'^`\"," :: String))

  case runParser buttonP "" (T.singleton c) of
    Left  _ -> fail $ "Internal error: Could not parse deadkey sequence `" <> [c] <> "`"
    Right b -> pure [b]

-- | Parse any button
buttonP :: Parser DefButton
buttonP = buttonP' False

buttonP' :: Bool -> Parser DefButton
buttonP' toplevel = mkTokenP' toplevel keywordButtons <|> choice noKeywordButtons <?> "button"

-- | Parsers for buttons that have a keyword at the start; the format is
-- @(keyword, how to parse the token)@
keywordButtons :: [(Text, Parser DefButton)]
keywordButtons =
  [ (,) "around-implicit"          $ KAroundImplicit        <$> buttonP  <*> buttonP
  , (,) "press-only"               $ KPressOnly             <$> keycodeP
  , (,) "release-only"             $ KReleaseOnly           <$> keycodeP
  , (,) "multi-tap"                $ KMultiTap              <$> timed    <*> buttonP
  , (,) "stepped"                  $ KStepped               <$> buttonsP
  , (,) "tap-hold"                 $ KTapHold               <$> numP     <*> buttonP <*> buttonP
  , (,) "tap-hold-next"            $ KTapHoldNext           <$> numP     <*> buttonP <*> buttonP <*> timeoutP
  , (,) "tap-next-release"         $ KTapNextRelease        <$> graceP   <*> buttonP <*> buttonP
  , (,) "tap-hold-next-release"    $ KTapHoldNextRelease    <$> numP     <*> graceP  <*> buttonP <*> buttonP <*> timeoutP
  , (,) "tap-next-press"           $ KTapNextPress          <$> buttonP  <*> buttonP
  , (,) "tap-hold-next-press"      $ KTapHoldNextPress      <$> numP     <*> buttonP <*> buttonP <*> timeoutP
  , (,) "tap-next"                 $ KTapNext               <$> buttonP  <*> buttonP
  , (,) "layer-toggle"             $ KLayerToggle           <$> word
  , (,) "momentary-layer"          $ KLayerToggle           <$> word
  , (,) "layer-switch"             $ KLayerSwitch           <$> word
  , (,) "permanent-layer"          $ KLayerSwitch           <$> word
  , (,) "layer-add"                $ KLayerAdd              <$> word
  , (,) "layer-rem"                $ KLayerRem              <$> word
  , (,) "layer-delay"              $ KLayerDelay            <$> numP     <*> word
  , (,) "layer-next"               $ KLayerNext             <$> word
  , (,) "around-next"              $ KAroundNext            <$> buttonP
  , (,) "around-next-single"       $ KAroundNextSingle      <$> buttonP
  , (,) "before-after-next"        $ KBeforeAfterNext       <$> buttonP  <*> buttonP
  , (,) "around-next-timeout"      $ KAroundNextTimeout     <$> numP     <*> buttonP <*> buttonP
  , (,) "tap-macro"                $ KTapMacro              <$> buttonsP <*> delayP
  , (,) "tap-macro-release"        $ KTapMacroRelease       <$> buttonsP <*> delayP
  , (,) "cmd-button"               $ KCommand               <$> textP    <*> optional textP
  , (,) "pause"                    $ kPause                 <$> numP
  , (,) "sticky-key"               $ KStickyKey             <$> numP     <*> buttonP
  ]
  ++ map (\(nm,_,btn) -> (nm, btn <$> buttonP <*> buttonP)) implArndButtons
 where
  timed :: Parser [(Int, DefButton)]
  timed = many ((,) <$> numP <*> buttonP)
  kPause = KPause . fromIntegral
  buttonsP = some buttonP
  timeoutP = optarg'P "timeout-button" buttonP
  graceP = optarg'P "grace" numP
  delayP   = optarg'P "delay" numP

implArndButtons :: [(Text, ImplArnd, DefButton -> DefButton -> DefButton)]
implArndButtons =
  [ ("around"           , IAAround         , KAround)
  , ("around-only"      , IAAroundOnly     , KAroundOnly)
  , ("around-when-alone", IAAroundWhenAlone, KAroundWhenAlone)
  ]

-- | Parsers for buttons that do __not__ have a keyword at the start
noKeywordButtons :: [Parser DefButton]
noKeywordButtons =
  [ KComposeSeq <$> deadkeySeqP
  , rmTapMacroP
  , fromNamed buttonNames
  , KRef  <$> derefP
  , moddedP
  , pauseP
  , KEmit <$> keycodeP
  , KComposeSeq <$> composeSeqP
  ]

--------------------------------------------------------------------------------
-- $defcfg

-- | Parse an input token
itokenP :: Parser IToken
itokenP = mkTokenP itokens

-- | Input tokens to parse; the format is @(keyword, how to parse the token)@
itokens :: [(Text, Parser IToken)]
itokens =
  [ ("device-file"   , KDeviceSource . unpack <$> textP)
  , ("low-level-hook", pure KLowLevelHookSource)
  , ("iokit-name"    , KIOKitSource <$> optional textP)
  ]

-- | Parse an output token
otokenP :: Parser OToken
otokenP = mkTokenP otokens

-- | Output tokens to parse; the format is @(keyword, how to parse the token)@
otokens :: [(Text, Parser OToken)]
otokens =
  [ ("uinput-sink"    , KUinputSink <$> textP <*> optional textP)
  , ("send-event-sink", KSendEventSink <$> optional ((,) <$> numP <*> numP))
  , ("kext"           , pure KKextSink)
  ]

-- | Parse an impl arnd token
implArndP :: Parser ImplArnd
implArndP = choice $
  keyword' "disabled" IADisabled
  : map (\(s, v, _) -> keyword' s v) implArndButtons

-- | Parse the DefCfg token
defcfgP :: Parser DefSettings
defcfgP = some settingP

-- | All possible configuration options that can be passed in the defcfg block
settingP :: Parser DefSetting
settingP =
  choice
    [ SIToken      <$> sexpr "input"         itokenP
    , SOToken      <$> sexpr "output"        otokenP
    , SCmpSeq      <$> sexpr "cmp-seq"       buttonP
    , SFallThrough <$> sexpr "fallthrough"   boolP
    , SAllowCmd    <$> sexpr "allow-cmd"     boolP
    , SCmpSeqDelay <$> sexpr "cmp-seq-delay" numP
    , SKeySeqDelay <$> sexpr "key-seq-delay" numP
    , SImplArnd    <$> sexpr "implicit-around" implArndP
    ]

--------------------------------------------------------------------------------
-- $defalias

-- | Parse a collection of names and buttons
defaliasP :: Parser DefAlias
defaliasP = many $ (,) <$> word <*> buttonP

--------------------------------------------------------------------------------
-- $defsrc

defsrcP :: Parser DefSrc
defsrcP =
  DefSrc
    <$> optarg'P "name" word
    <*> many buttonP

--------------------------------------------------------------------------------
-- $deflayer

deflayerP :: Parser DefLayer
deflayerP = DefLayer <$> word <*> many layerSettingP

layerSettingP :: Parser DefLayerSetting
layerSettingP =
  choice
    [ LSrcName   <$> optargP "source" word
    , LImplArnd  <$> optargP "implicit-around" implArndP
    , LButton    <$> buttonP
    ]
