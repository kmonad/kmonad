module KMonad.Domain.Effect.Effects.MonadButton
  ( MonadButton(..)
  , press, release, tap
  )
where

import KMonad.Core
import KMonad.Domain.Button.Button

class Monad m => MonadButton m where
  runButton :: Button m -> SwitchState -> m ()

press :: MonadButton m => Button m -> m ()
press b = runButton b Engaged

release :: MonadButton m => Button m -> m ()
release b = runButton b Disengaged

tap :: MonadButton m => Button m -> m ()
tap b = press b *> release b
