module KMonad.Domain.Effect.Effects.MonadInject
  ( MonadInject(..)
  )
where

import KMonad.Core

-- | This effect allows the injection of internally generated 'Event's that will
-- be pulled by 'nextEvent'. This is the mechanism by which we can cleanly
-- shutdown KMonad (by passing a 'KMonad.Core.Event.Quit' event).
class Monad m => MonadInject m where
  injectEvent :: Event -> m ()
