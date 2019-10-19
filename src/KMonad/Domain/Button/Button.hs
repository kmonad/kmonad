module KMonad.Domain.Button.Button

where

import UnliftIO

import KMonad.Core


--------------------------------------------------------------------------------
-- $types

-- | Buttons are actions in a particular context that are triggered by
-- switch-states.
--
-- @since 0.1.0
newtype Button m = Button { unButton :: (SwitchState -> m (), MVar SwitchState)}

-- | Create a new 'Button' from an effectful function from 'SwitchState'
--
-- @since 0.1.0
mkButton :: (MonadIO io, Monad m) => (SwitchState -> m ()) -> io (Button m)
mkButton f = Button . (f,) <$> liftIO (newMVar Disengaged)

-- | Create a new 'Button' that performs an action when pressed and does nothing
-- when released
onPress :: (MonadIO io, Monad m) => m () -> io (Button m)
onPress a = mkButton $ \case
  Engaged    -> a
  Disengaged -> pure ()

runButtonIO :: (MonadIO io, Monad m) => Button m -> SwitchState -> io (m ())
runButtonIO b x = do
  let (f, v) = unButton b
  st <- takeMVar v
  case (st, x) of
    (Engaged, Disengaged) -> putMVar v Disengaged >> pure (f Disengaged)
    (Disengaged, Engaged) -> putMVar v Engaged    >> pure (f Engaged)
    _                     -> putMVar v st         >> pure (pure ())
