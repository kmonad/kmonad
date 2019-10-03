{-|
Module      : KMonad.Core.Parser.Parsers.KeySequence
Description : Parsing sequences of KeyAction's
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Core.Parser.Parsers.KeyAction
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
