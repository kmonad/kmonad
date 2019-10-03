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
  -- ( -- * Types and lenses for KeyEvents
  --   -- $types
  --   KeyEventType(..)
  -- , KeyEvent(..)
  -- , AsKeyEvent(..)
  -- , eventType, keyCode

  --   -- * Dealing especially with /LockingKeys/
  --   -- $locks
  -- , LockKey(..)
  -- , LockState
  -- , LockUpdate
  -- , emptyLockState
  -- , addLock , deleteLock , toggleLock

  --   -- * Creating sequences of KeyEvents
  --   -- $seqs
  -- , KeySequence
  -- , press, release, repeat, tap

  --   -- * Comparing events
  --   -- $comps
  -- , EventComparison
  -- , compareEvent
  -- , since, sameCode
  -- )
where

import Prelude hiding (repeat)

import Control.Lens

import KMonad.Core.KeyCode
import KMonad.Core.Time
import KMonad.Core.Types

import qualified Data.Set as S


--------------------------------------------------------------------------------
-- $action

-- | The KeyActionType that distinguishes press, release and repeat events
data KeyActionType
  = Press
  | Release
  | Repeat
  deriving (Eq, Show)
makeClassyPrisms ''KeyActionType

-- | A KeyAction is a Press, Release, or Repeat of a KeyCode
data KeyAction = KeyAction
  { _kaType    :: KeyActionType
  , _kaKeyCode :: KeyCode
  } deriving (Eq, Show)
makeClassy ''KeyAction

instance HasType KeyAction KeyActionType where
  _type = kaType
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
instance HasKeyAction KeyEvent          where keyAction = evKeyAction
instance HasType KeyEvent KeyActionType where _type = keyAction._type
instance HasKeyCode KeyEvent            where keyCode = keyAction.keyCode
instance HasTime KeyEvent               where time = evTime

-- | Create a KeyEvent
mkKeyEvent :: KeyActionType -> KeyCode -> Time -> KeyEvent
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

isPress, isRepeat, isRelease :: (HasType a KeyActionType) => a -> Bool
isPress   x = x^._type == Press
isRelease x = x^._type == Release
isRepeat  x = x^._type == Repeat


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
type LockUpdate = (KeySequence , LockState)

emptyLockState :: LockState
emptyLockState = S.empty

-- | The KeyCodes corresponding to the different LockKeys
codeForLock :: LockKey -> KeyCode
codeForLock ScrollLock = KeyScrollLock
codeForLock NumLock    = KeyNumLock
codeForLock CapsLock   = KeyCapsLock

-- | Tap a lock key
tapLock :: LockKey -> KeySequence
tapLock = tap . codeForLock

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
press, release, repeat :: KeyCode -> KeyAction
press   = KeyAction Press
release = KeyAction Release
repeat  = KeyAction Repeat

-- | Aliases for `press` and `release`
kP, kR :: KeyCode -> KeyAction
kP = press
kR = release

-- | Create a 'KeyEvent' sequence that presses and then releases a button
tap :: KeyCode -> KeySequence
tap c = [kP c, kR c]


--------------------------------------------------------------------------------
-- $comps
--
-- For some 'KMonad.Core.Button.Button' actions we need to be able to know
-- whether the next 'KeyEvent' is a release of the same button, or perhaps the
-- press of another. But since buttons are expressly isolated from their
-- context, we provide a mechanism for gathering information about future
-- 'KeyEvent's. Using the 'KMonad.Domain.Effect.Future.MonadFuture' class,
-- buttons get access to data of the 'EventComparison' type.

-- | The EventComparison record
data EventComparison = EventComparison
  { _sameCode     :: Bool          -- ^ Whether or not both 'KeyEvent's share the same 'KeyCode'
  , _since        :: Nanoseconds   -- ^ The time between the two events
  , _cmpEventType :: KeyActionType -- ^ The event-type of the second event
  } deriving (Eq, Show)
makeClassy ''EventComparison

instance HasType EventComparison KeyActionType where _type = cmpEventType

-- | Return an 'EventComparison' between two events, if the first event occured
-- before the second, the 'since' value will be positive, otherwise negative.
compareEvent :: KeyEvent -> KeyEvent -> EventComparison
compareEvent a b = EventComparison
  { _sameCode     = a^.keyCode == b^.keyCode
  , _since        = (a^.time) `tminus` (b^.time)
  , _cmpEventType = b^._type
  }
