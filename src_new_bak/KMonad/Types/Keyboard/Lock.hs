{-|
Module      : KMonad.Types.Keyboard.Lock
Description : Special support for different xxxLock style keys
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Types.Keyboard.Lock
  ( -- * Locking key support
    -- $locks
    LockKey(..)
  , LockState
  , LockUpdate
  , emptyLockState
  , addLock , deleteLock , toggleLock
  )
where

import Prelude

import KMonad.Types.Keyboard.Keycode
import KMonad.Types.Keyboard.KeySequence

import qualified RIO.Set as S

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
codeForLock :: LockKey -> Keycode
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
