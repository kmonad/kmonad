module KMonad.Domain.Effect.Effects.MonadHold
  ( MonadHold(..)
  , holdDuring
  )
where

-- | This effect allows the pausing of handling button events. For example, a
-- 'KMonad.Core.Parser.Token.BTapHold' pauses processing until it decides what
-- action to take, and then resumes processing again.
class Monad m => MonadHold m where
  hold :: Bool -> m ()

-- | Put a hold on processing while performing an action.
holdDuring :: MonadHold m => m a -> m a
holdDuring a = do
  hold True
  x <- a
  hold False
  return x
