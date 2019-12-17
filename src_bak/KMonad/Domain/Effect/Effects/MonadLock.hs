module KMonad.Domain.Effect.Effects.MonadLock
  ( MonadLock(..)
  )
where

import KMonad.Core


-- | This allows for the toggling of keyboard locking keys while keeping track
-- of the state internally.
class Monad m => MonadLock m where
  lockOn     :: LockKey -> m ()
  lockOff    :: LockKey -> m ()
  lockToggle :: LockKey -> m ()
