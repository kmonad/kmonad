module KMonad.Domain.Effect.Effects.MonadMaskInput
  ( MonadMaskInput(..)
  )
where


class Monad m => MonadMaskInput m where
  -- | Mask events with the same keycode as the current event from being
  -- processed, and return the action to unmask processing again.
  maskInput :: m (m ())
