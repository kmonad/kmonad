module KMonad.Button

where

import Prelude

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
  deriving Show


--------------------------------------------------------------------------------
-- $button

-- | The configurable aspects of a 'Button'
data ButtonCfg = ButtonCfg
  { _pressAction   :: !Action -- ^ Action to take when pressed
  , _releaseAction :: !Action -- ^ Action to take when released
  } deriving (Show)
makeClassy ''ButtonCfg

-- | The unique state identifying 1 'button'
data Button = Button
  { _binding    :: !Keycode             -- ^ The 'Keycode' to which the button is bound
  , _beButtonCfg  :: !ButtonCfg           -- ^ The configuration for this button
  , _lastAction :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
  }
makeLenses ''Button

instance HasButtonCfg Button where buttonCfg = beButtonCfg

-- | Initialize a 'Button' from a 'ButtonCfg' and a binding
initButton :: Keycode -> ButtonCfg -> RIO e Button
initButton kc cfg = Button kc cfg <$> newMVar Release

-- | Run a 'Button' on a 'SwitchAction' returning an 'Action' to be performed by
-- the engine. This will only do something if a 'Press' followed a 'Release' or
-- vice-versa, pressing or releasing more than once in sequence does nothing.
runButton :: SwitchAction -> Button -> RIO e Action
runButton a b = do
  modifyMVar (b^.lastAction) $ \l -> pure $ case (a, l) of
    (Press, Release) -> (Release, b^.pressAction)
    (Release, Press) -> (Press, b^.releaseAction)
    _                -> (a, Pass)

-- | Press the 'Button'
pressButton :: Button -> RIO e Action
pressButton = runButton Press

-- | Release the 'Button'
releaseButton :: Button -> RIO e Action
releaseButton = runButton Release

 
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

runPriorityButton :: PriorityButton -> RIO e Action
runPriorityButton = undefined

