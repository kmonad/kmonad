{-|
Module      : KMonad.Domain.Button.Buttons.Lockers
Description : A button that adds a layer to the top of the stack
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

Buttons to toggle the 3 `locking` style buttons.

-}
module KMonad.Domain.Button.Buttons.Lockers
  ( mkLockOn
  , mkLockOff
  , mkLockToggle
  )
where

import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Domain.Button.Button

-- | Return a button that turns a lock On
mkLockOn :: (MonadIO io, MonadLock m)
  => LockKey
  -> io (Button m)
mkLockOn lk = mkButton $ \case
  Engaged    -> lockOn lk
  Disengaged -> pure ()

-- | Return a button that turns a lock Off
mkLockOff :: (MonadIO io, MonadLock m)
  => LockKey
  -> io (Button m)
mkLockOff lk = mkButton $ \case
  Engaged    -> lockOff lk
  Disengaged -> pure ()

-- | Return a button that toggles a lock
mkLockToggle :: (MonadIO io, MonadLock m)
  => LockKey
  -> io (Button m)
mkLockToggle lk = mkButton $ \case
  Engaged    -> lockToggle lk
  Disengaged -> pure ()
