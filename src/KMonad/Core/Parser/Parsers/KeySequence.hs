{-|
Module      : KMonad.Core.Parser.Parsers.KeySequence
Description : Parsing sequences of KeyAction's
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Core.Parser.Parsers.KeySequence
  ( keySequence
  , shifted
  , modded
  , seqElem
  , tapP
  )
where


import KMonad.Core.KeyCode
import KMonad.Core.Keyboard

import KMonad.Core.Parser.Parsers.KeyCode
import KMonad.Core.Parser.Utility


--------------------------------------------------------------------------------

keySequence :: Parser KeySequence
keySequence = keySequenceNew <|> keySequenceOld


-- | Parse any sequence of keypresses between '((' and '))' Keycodes are
-- interpreted as taps, keycodes preceded by a P are presses, and by an R are
-- releases. For example:
--
-- >>> "Plsft a b c Rlsft d e"
-- would correspond to typing ABCde
keySequenceNew :: Parser KeySequence
keySequenceNew = do
  _  <- symbol "(("
  es <- concat <$> many (lexemeSameLine $ seqElem <|> shifted <|> modded)
  _  <- symbol "))"
  return es


{-# DEPRECATED keySequenceOld "Support for || a b c || macros to end soon, use (( a b c ))" #-}
keySequenceOld :: Parser KeySequence
keySequenceOld = do
  _  <- symbol "||"
  es <- concat <$> many (lexemeSameLine $ seqElem <|> shifted <|> modded)
  _  <- symbol "||"
  return es

seqElem :: Parser KeySequence
seqElem = choice [try pressSeqP, try releaseSeqP, tapP, shifted, modded]

-- | Parse a Press action
pressP :: Parser KeyAction
pressP = char 'P' *> (mkKeyPress <$> keycodeP)

-- | Parse a Release action
releaseP :: Parser KeyAction
releaseP = char 'R' *> (mkKeyRelease <$> keycodeP)

-- | Parse a Press action as a sequence of 1
pressSeqP :: Parser KeySequence
pressSeqP = (:[]) <$> pressP

-- | Parse a Release action as a sequence of 1
releaseSeqP :: Parser KeySequence
releaseSeqP = (:[]) <$> releaseP

-- | Parse a Tap as a press and release
tapP :: Parser KeySequence
tapP = (\c -> [mkKeyPress c, mkKeyRelease c]) <$> keycodeP

--------------------------------------------------------------------------------

-- | Parse emacs-style mod-notation as a key-sequence

-- | Parse any "S-button" or "C-button" style indicator as a modded button. This
-- is recursive, so "C-S-button" works fine too
modded :: Parser KeySequence
modded = do
  mod' <- choice [ KeyLeftShift  <$ string "S-"
                 , KeyLeftCtrl   <$ string "C-"
                 , KeyLeftAlt    <$ string "A-"
                 , KeyLeftMeta   <$ string "M-"
                 , KeyRightShift <$ string "RS-"
                 , KeyRightCtrl  <$ string "RC-"
                 , KeyRightAlt   <$ string "RA-"
                 , KeyRightMeta  <$ string "RM-"
                 , KeyCompose    <$ string "CMP-"
                 ] <?> "Mod sequence"
  rest <- keySequence <|> modded <|> shifted <|> tapP <|> pressSeqP <|> releaseSeqP
  return $ around mod' rest

-- | Parse a number of special characters as "the shifted sequence to push
-- them". So "!" gets parsed to a shifted 1, for example. This does not include
-- any capital letters, since those signify special characters.
shifted :: Parser KeySequence
shifted = (fromNamed m <* notFollowedBy alphaNumChar)
  where
    s kc = kP KeyLeftShift <> mkKeyTap kc <> kR KeyLeftShift
    m = [ ( "!",  s Key1)
        , ( "@",  s Key2)
        , ( "#",  s Key3)
        , ( "$",  s Key4)
        , ( "%",  s Key5)
        , ( "^",  s Key6)
        , ( "&",  s Key7)
        , ( "*",  s Key8)
        , ( "(",  s Key9)
        , ( ")",  s Key0)
        , ( "__", s KeyMinus)
        , ( "<",  s KeyComma)
        , ( ">",  s KeyDot)
        , ( "{",  s KeyLeftBrace)
        , ( "}",  s KeyRightBrace)
        , ( "?",  s KeySlash)
        , ( "|",  s KeyBackslash)
        , ( ":",  s KeySemicolon)
        , ( "\"", s KeyApostrophe)
        , ( "~",  s KeyGrave)
        , ( "+",  s KeyEqual)
        , ( "A",  s KeyA)
        , ( "B",  s KeyB)
        , ( "C",  s KeyC)
        , ( "D",  s KeyD)
        , ( "E",  s KeyE)
        , ( "F",  s KeyF)
        , ( "G",  s KeyG)
        , ( "H",  s KeyH)
        , ( "I",  s KeyI)
        , ( "J",  s KeyJ)
        , ( "K",  s KeyK)
        , ( "L",  s KeyL)
        , ( "M",  s KeyM)
        , ( "N",  s KeyN)
        , ( "O",  s KeyO)
        , ( "P",  s KeyP)
        , ( "Q",  s KeyQ)
        , ( "R",  s KeyR)
        , ( "S",  s KeyS)
        , ( "T",  s KeyT)
        , ( "U",  s KeyU)
        , ( "V",  s KeyV)
        , ( "W",  s KeyW)
        , ( "X",  s KeyX)
        , ( "Y",  s KeyY)
        , ( "Z",  s KeyZ)
        ]
