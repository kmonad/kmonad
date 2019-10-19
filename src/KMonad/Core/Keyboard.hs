{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Core.Keyboard
Description : Types and utilities for 'KeyEvent's
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This module contains the representation of keyboard events as used by KMonad. We
currently only represent press, release, and repeat events that have already
been assigned a keycode. This means we cannot deal with sync events, treating
every seperate event as an atomic update to the keyboard state, and we cannot
deal with scan-codes.

Although we do interpret 'Repeat' events and represent them internally in
KMonad, 'KMonad.Core.Button.Button's are not equipped to deal with them, and all
'Repeat' events are discarded by the handling phase. Instead of dealing with
'Repeat' events, we simply emit 'Press' and 'Release' events, and let the OS
deal with how to generate repeating events.

-}
module KMonad.Core.Keyboard
  ( -- * Types for KeyActions
    KeyAction(..)

    -- * Types and lenses for KeyEvents
    -- $types
  , KeyEvent
  , mkKeyEvent
  , AsKeyEvent(..)
  , _type, keyCode
  , isPress, isRelease
  , actAtTime

    -- * Dealing especially with /LockingKeys/
    -- $locks
  , LockKey(..)
  , LockState
  , LockUpdate
  , emptyLockState
  , addLock , deleteLock , toggleLock

    -- * Creating sequences of KeyEvents
    -- $seqs
  , KeySequence
  , mkKeyPress, mkKeyRelease, mkKeyTap
  , kR, kP, around

    -- * Comparing events
    -- $comps
  -- , EventComparison
  -- , compareEvent
  -- , since, sameCode
  )
where

import Prelude hiding (repeat)

import Control.Lens

import KMonad.Core.KeyCode
import KMonad.Core.Switch
import KMonad.Core.Time
import KMonad.Core.Types

import qualified Data.Set as S


--------------------------------------------------------------------------------
-- $action

-- | A KeyAction is either a press or release of a keycode

data KeyAction = KeyAction
  { _kaSwitchState :: SwitchState -- ^ Switch-state after KeyAction is completed
  , _kaKeyCode     :: KeyCode     -- ^ The corresponding KeyCode
  } deriving (Eq, Show)
makeClassy ''KeyAction

instance Ord KeyAction where
  a `compare` b = case (a^.switchState) `compare` (b^.switchState) of
    EQ -> (a^.keyCode) `compare` (b^.keyCode)
    x  -> x


instance HasSwitchState KeyAction where
  switchState = kaSwitchState

instance HasKeyCode KeyAction where
  keyCode = kaKeyCode


--------------------------------------------------------------------------------
-- $event

-- | A KeyEvent is a KeyAction that occurred at a particular time
data KeyEvent = KeyEvent
  { _evKeyAction :: KeyAction
  , _evTime      :: Time
  } deriving (Eq, Show)
makeClassy ''KeyEvent

-- Hook up all the classy lenses
instance HasKeyAction   KeyEvent where keyAction   = evKeyAction
instance HasSwitchState KeyEvent where switchState = keyAction.switchState
instance HasKeyCode     KeyEvent where keyCode     = keyAction.keyCode
instance HasTime        KeyEvent where time        = evTime

-- | Create a KeyEvent
mkKeyEvent :: SwitchState -> KeyCode -> Time -> KeyEvent
mkKeyEvent a c = KeyEvent (KeyAction a c)

-- | Create a KeyEvent by running a KeyAction at a given time
actAtTime :: KeyAction -> Time -> KeyEvent
actAtTime = KeyEvent

-- | A prism describing something that can be cast to a KeyEvent. This is used
-- in the concrete implementation of Linux KeyIO, converting raw event tuples to
-- 'KeyEvent's when reading, and 'KeyEvent's to raw event tuples when writing.
-- If you extend KMonad by adding different KeyIO facilities, write an instance
-- of 'AsKeyEvent' for your concrete event.
class AsKeyEvent s where
  _KeyEvent :: Prism' s KeyEvent
instance AsKeyEvent KeyEvent where
  _KeyEvent = prism' id Just

--------------------------------------------------------------------------------
-- $util

isPress, isRelease :: (HasSwitchState a) => a -> Bool
isPress   x = x^.switchState == Engaged
isRelease x = x^.switchState == Disengaged

--------------------------------------------------------------------------------
-- $locks
--
-- We provide some extra functionality to deal differently with the 3 classical
-- 'locking' keys. This is to facilitate being able to differentiate between
-- "activating a lock", "releasing a lock", or "toggling a lock".

-- | ADT representing the 3 different 'locking keys' that can exist on keyboards
data LockKey
  = ScrollLock
  | NumLock
  | CapsLock
  deriving (Eq, Show, Ord)

-- | A set describing the locks that are currently engaged
type LockState = S.Set LockKey

-- | The update required to update both the OS and the internal LockState
-- representation
type LockUpdate = (KeySequence, LockState)

emptyLockState :: LockState
emptyLockState = S.empty

-- | The KeyCodes corresponding to the different LockKeys
codeForLock :: LockKey -> KeyCode
codeForLock ScrollLock = KeyScrollLock
codeForLock NumLock    = KeyNumLock
codeForLock CapsLock   = KeyCapsLock

-- | Tap a lock key
tapLock :: LockKey -> KeySequence
tapLock = mkKeyTap . codeForLock

-- | Switch a lock to on, or do nothing if already on
addLock :: LockKey -> LockState -> LockUpdate
addLock k st = if S.member k st
  then ([], st)
  else (tapLock k, S.insert k st)

-- | Switch a lock to off, or do nothing if already off
deleteLock :: LockKey -> LockState -> LockUpdate
deleteLock k st = if S.member k st
  then (tapLock k, S.delete k st)
  else ([], st)

-- | Toggle a lock from on to off or vice versa
toggleLock :: LockKey -> LockState -> LockUpdate
toggleLock k st = (tapLock k,) $ if S.member k st
  then S.delete k st else S.insert k st


--------------------------------------------------------------------------------
-- $seqs

type KeySequence =  [KeyAction]

-- | Create a KeySequence containing 1 event
-- one :: KeyEventType -> KeyCode -> KeySequence
-- one e c = \t -> [KeyEvent e c t]

-- | Create a KeyAction with the provided KeyCode
mkKeyPress, mkKeyRelease :: KeyCode -> KeyAction
mkKeyPress   = KeyAction Engaged
mkKeyRelease = KeyAction Disengaged

-- | Aliases for `mkKeyPress` and `mkKeyRelease` that return sequences
kP, kR :: KeyCode -> KeySequence
kP = (:[]) . mkKeyPress
kR = (:[]) . mkKeyRelease

-- | Create a 'KeyEvent' sequence that presses and then releases a button
mkKeyTap :: KeyCode -> KeySequence
mkKeyTap c = [mkKeyPress c, mkKeyRelease c]

around :: KeyCode -> KeySequence -> KeySequence
around kc sq = kP kc <> sq <> kR kc
