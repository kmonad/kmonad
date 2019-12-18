module KMonad.Domain.Types

where

import KMonad.Prelude

import KMonad.Core

--------------------------------------------------------------------------------
-- $action

data Action
  = Emit KeyAction
  | Pause Milliseconds
  | Race (Action, Action) (Action, Action)
  -- | Hold Bool
  -- | WaitFor (KeyEvent -> Bool)
  | Macro [Action]
  -- | MaskInput Bool
  | Fork Action
  | Pass

--------------------------------------------------------------------------------
-- $button

-- | The environment in which 'Button' actions are run
data Button = Button
  { _binding       :: !Keycode             -- ^ The 'Keycode' to which the button is bound
  , _pressAction   :: !Action              -- ^ Action to take when pressed
  , _releaseAction :: !Action              -- ^ Action to take when released
  , _lastAction    :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
  }
makeClassy ''Button

-- | Create a 'Button' from a 'Keycode' to which to bind it, and two 'Action's.
mkButton :: MonadIO m
  => Action   -- ^ The 'Action' to take when the 'Button' is pressed
  -> Action   -- ^ The 'Action' to take when the 'Button' is released
  -> Keycode  -- ^ The 'Keycode' to which the 'Button' is bound
  -> m Button -- ^ An action that provides resulting 'Button'
mkButton onP onR kc = Button kc onP onR <$> newMVar Release

