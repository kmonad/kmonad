module KMonad.Domain.Effect.Effects.MonadFork
  ( MonadFork(..))
where


-- | The 'MonadFork' effect allows forking operations
class Monad m => MonadFork m where
  fork :: m () -> m ()
