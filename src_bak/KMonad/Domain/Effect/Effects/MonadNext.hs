module KMonad.Domain.Effect.Effects.MonadNext
  ( MonadNext(..)
  )
where

import KMonad.Core

-- | An API must implement how to perform a blocking call awaiting the next
-- event to occur.
class Monad m => MonadNext m where
  nextEvent :: m Event
