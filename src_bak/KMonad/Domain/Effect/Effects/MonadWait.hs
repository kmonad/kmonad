module KMonad.Domain.Effect.Effects.MonadWait
  ( MonadWait(..)
  )
where

import KMonad.Core.Time

-- | Allow an action to wait for a bit.
class Monad m => MonadWait m where
  wait :: Microseconds -> m ()
