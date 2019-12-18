module KMonad.Domain.Types

where

import KMonad.Prelude

import Data.Serialize

import KMonad.Core

--------------------------------------------------------------------------------
-- $action
--
-- 'Action's are an AST of different actions that can be performed by 'Button's
-- on their press or release events.


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
--
-- 'Button's are 2 different 'Action's bound to the press and release events of
-- a particular 'Keycode'. 'Button's are structured so that only
-- state-transitions are effectful (press after press does nothing, release
-- after release does nothing).

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


--------------------------------------------------------------------------------
-- $event
--
-- 'Event's are the collection of all things that can trigger KMonad to update.

-- | An 'Event' is either a 'KeyEvent' or a signal to shutdown
data Event
  = InputEvent   KeyEvent   -- ^ A 'KeyEvent' is registered from the 'KeySource'
  | MessageEvent Message    -- ^ A 'Message' is received through the network socket
  | Quit                    -- ^ A 'Quit' event is sent from somewhere inside KMonad
  deriving (Eq, Show, Generic)

instance Serialize Event

--------------------------------------------------------------------------------
-- $msg
--
-- 'Message's are the collection of messages that can be sent to the KMonad
-- daemon through its messaging socket.


-- | The different 'Message's that can be sent to KMonad
data Message
  = Shutdown          -- ^ Signal to quit
  | EmitKey KeyAction -- ^ Signal to emit a key action to the OS
  deriving (Eq, Show, Generic)

instance Serialize Message
