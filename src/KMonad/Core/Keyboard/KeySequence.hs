{-|
Module      : KMonad.Core.Keyboard.KeySequence
Description : Sequences of KeyActions
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Core.Keyboard.KeySequence
  ( -- * Creating sequences of KeyEvents
    -- $seqs
    KeySequence
  , mkKeyPress, mkKeyRelease, mkKeyTap
  , kR, kP, around
  )
where

import KMonad.Prelude

import KMonad.Core.Keyboard.Event
import KMonad.Core.Keyboard.Keycode

--------------------------------------------------------------------------------
-- $seqs

type KeySequence =  [KeyAction]

-- | Create a KeyAction with the provided Keycode
mkKeyPress, mkKeyRelease :: Keycode -> KeyAction
mkKeyPress   = KeyAction Press
mkKeyRelease = KeyAction Release

-- | Aliases for `mkKeyPress` and `mkKeyRelease` that return sequences
kP, kR :: Keycode -> KeySequence
kP = (:[]) . mkKeyPress
kR = (:[]) . mkKeyRelease

-- | Create a 'KeyEvent' sequence that presses and then releases a button
mkKeyTap :: Keycode -> KeySequence
mkKeyTap c = [mkKeyPress c, mkKeyRelease c]

-- | Create a sequence by pressing, then running a sequence, and then releasing
around :: Keycode -> KeySequence -> KeySequence
around kc sq = kP kc <> sq <> kR kc
