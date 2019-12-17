module KMonad.Domain.Effect.Effects.MonadFuture
  ( MonadFuture(..)
  , waitFor
  , waitForM
  , waitForWith
  , waitForWithM
  )
where


import KMonad.Core

-- | FIXME: documentation
class Monad m => MonadFuture m where
  waitNext :: m (m KeyEvent)

-- | Given a predicate on EventComparisons, return () once the predicate is
-- satisfied.
--
-- This is often used in conjunction with 'race' to do something in the case
-- some condition is met within a given time.
-- For example:
--
-- >>> race (wait 1000000) (waitFor (\c -> (c^.eventType == Release)) >> doThing)
--
waitFor :: MonadFuture m => (KeyEvent-> Bool) -> m ()
waitFor p = waitNext >>= waitForWith p

waitForM :: MonadFuture m => (KeyEvent-> m Bool) -> m ()
waitForM p = waitNext >>= waitForWithM p

-- | Like waitFor, but using an already pinned action
--
-- `waitFor p` is equivalent to `pinComparison >>= waitForWith p`
waitForWith :: Monad m
  => (a -> Bool) -- ^ The predicate to match
  -> m a         -- ^ The action that generates comparisons
  -> m ()
waitForWith p nxt = nxt >>= \cmp -> if p cmp then pure () else waitForWith p nxt

waitForWithM :: Monad m
  => (a -> m Bool) -- ^ The effectful operation to use for matching
  -> m a           -- ^ The action that generates comparisons
  -> m ()
waitForWithM p nxt = nxt >>= \cmp -> p cmp >>= \case
  True  -> pure ()
  False -> waitForWithM p nxt
