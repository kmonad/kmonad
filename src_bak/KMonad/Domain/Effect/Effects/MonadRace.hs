module KMonad.Domain.Effect.Effects.MonadRace
  ( MonadRace(..)
  )
where


-- | This effect allows for racing two different actions to see which finishes
-- first. The moment 1 action finishes, the other is cancelled.
class Monad m => MonadRace m where
  race :: m a -> m b -> m (Either a b)
