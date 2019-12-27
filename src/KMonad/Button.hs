module KMonad.Button

where

import KMonad.Prelude

import KMonad.Keyboard
import KMonad.Util


--------------------------------------------------------------------------------
-- $action
--
-- 'Action's are an AST of different actions that can be performed by 'Button's
-- on their press or release events.

data Action
  = Emit  !KeyAction
  | Pause !Milliseconds
  | Race !(Action, Action) !(Action, Action)
  | Hold Bool
  -- | WaitFor (KeyEvent -> Bool)
  | Macro ![Action]
  -- | MaskInput Bool <-- Maybe make a 'register handler' function
  | Fork !Action
  | Pass


--------------------------------------------------------------------------------
-- $button

-- | The configurable aspects of a 'Button'
data ButtonCfg = ButtonCfg
  { _pressAction   :: !Action -- ^ Action to take when pressed
  , _releaseAction :: !Action -- ^ Action to take when released
  }
makeClassy ''ButtonCfg

-- | The unique state identifying 1 'button'
data Button = Button
  { _beBinding    :: !Keycode             -- ^ The 'Keycode' to which the button is bound
  , _beLastAction :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
  , _beButtonCfg  :: !ButtonCfg           -- ^ The configuration for this button
  }
makeLenses ''Button

runButton :: KeyAction -> Button -> RIO e ()
runButton = undefined

--------------------------------------------------------------------------------
-- $priority
--
-- The 'PriorityButton' is a special button that doesn't live in the standard
-- 'KeyMap', instead it is checked before any 'KeyMap' lookup is attempted. This
-- allows 'Button's to bypass the normal lookup behavior to pause and resume
-- processing.

data PriorityButton = PriorityButton
  { _trigger     :: KeyAction
  , _action    :: Action
  -- , _phButtonCfg :: ButtonCfg
  }
makeClassy ''PriorityButton

runPriorityButton :: PriorityButton -> RIO e ()
runPriorityButton = undefined



data ButtonEnv e = ButtonEnv
  { _emit :: KeyAction -> RIO e ()
  , _this :: Button
  }
