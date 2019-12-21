module KMonad.Types.Action

where

import KMonad.Types.Keyboard.Event
import KMonad.Types.Time


--------------------------------------------------------------------------------
-- $action
--
-- 'Action's are an AST of different actions that can be performed by 'Button's
-- on their press or release events.


data Action
  = Emit  !KeyAction
  | Pause !Milliseconds
  | Race !(Action, Action) !(Action, Action)
  -- | Hold Bool
  -- | WaitFor (KeyEvent -> Bool)
  | Macro ![Action]
  -- | MaskInput Bool <-- Maybe make a 'register handler' function
  | Fork !Action
  | Pass
