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
  )
where


import KMonad.Core.KeyCode
import KMonad.Core.Keyboard

import KMonad.Core.Parser.Parsers.KeyCode
import KMonad.Core.Parser.Utility


--------------------------------------------------------------------------------

-- | Parse any sequence of keypresses. Keycodes are interpreted as taps,
-- keycodes preceded by a P are presses, and by an R are releases. For example:
--
-- >>> "Plsft a b c Rlsft d e"
-- would correspond to typing ABCde
keySequence :: Parser KeySequence
keySequence = concat <$> many (lexemeSameLine $ choice [pressSeqP, releaseSeqP, tapP])

-- | Parse a Press action
pressP :: Parser KeyAction
pressP = char 'P' *> (kP <$> keycodeP)

-- | Parse a Release action
releaseP :: Parser KeyAction
releaseP = char 'R' *> (kR <$> keycodeP)

-- | Parse a Press action as a sequence of 1
pressSeqP :: Parser KeySequence
pressSeqP = (:[]) <$> pressP

-- | Parse a Release action as a sequence of 1
releaseSeqP :: Parser KeySequence
releaseSeqP = (:[]) <$> releaseP

-- | Parse a Tap as a press and release
tapP :: Parser KeySequence
tapP = (\c -> [kP c, kR c]) <$> keycodeP

--------------------------------------------------------------------------------

-- | Parse emacs-style mod-notation as a key-sequence

-- | Parse any "S-button" or "C-button" style indicator as a modded button. This
-- is recursive, so "C-S-button" works fine too
-- modded :: Parser KeySequence
-- modded = do
--   mod <- choice [ KeyLeftShift  <$ string "S-"
--                 , KeyLeftCtrl   <$ string "C-"
--                 , KeyLeftAlt    <$ string "A-"
--                 , KeyLeftMeta   <$ string "M-"
--                 , KeyRightShift <$ string "RS-"
--                 , KeyRightCtrl  <$ string "RC-"
--                 , KeyRightAlt   <$ string "RA-"
--                 , KeyRightMeta  <$ string "RM-"
--                 , KeyCompose    <$ string "CMP-"
--                 ]
--   rest <- modded <|> keycodeP

  --   emit <|> macro <|> shifted
  -- return $ BModded mod rest
