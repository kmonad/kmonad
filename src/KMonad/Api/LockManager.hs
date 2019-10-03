{-|
Module      : KMonad.Api.LockManager
Description : Concrete implementation of managing a Lock-state
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Api.LockManager
  ( LockManager
  , HasLockManager(..)
  , mkLockManager
  , lmLockOn, lmLockOff, lmLockToggle
  )
where

import Control.Lens
import Control.Monad.Reader
import UnliftIO.MVar

import KMonad.Core
import KMonad.Domain.Effect

-- | The core LockManager type
data LockManager = LockManager
  { _lState :: MVar LockState }
makeClassy ''LockManager

-- | Return a new, empty LockManager
mkLockManager :: MonadIO m => m LockManager
mkLockManager = LockManager <$> newMVar emptyLockState

-- | Helper function
doOne :: ( HasLockManager r , MonadReader r m , MonadEmit m , MonadNow m , MonadIO m )
  => (LockKey -> LockState -> LockUpdate)
  -> LockKey
  -> m ()
doOne f k = do
  lm       <- view $ lockManager.lState
  (es, st) <- f k <$> takeMVar lm
  emitSeq es
  putMVar lm st


-- | Turn a lock on (if off)
lmLockOn :: (HasLockManager r, MonadEmit m, MonadNow m, MonadReader r m, MonadIO m)
  => LockKey -> m ()
lmLockOn = doOne addLock

-- | Turn a lock off (if on)
lmLockOff :: (HasLockManager r, MonadEmit m, MonadNow m, MonadReader r m, MonadIO m)
  => LockKey -> m ()
lmLockOff = doOne deleteLock

-- | Toggle a lock betweeen on and off
lmLockToggle :: (HasLockManager r, MonadEmit m, MonadNow m, MonadReader r m, MonadIO m)
  => LockKey -> m ()
lmLockToggle = doOne toggleLock
