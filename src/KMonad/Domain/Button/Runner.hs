module KMonad.Domain.Button.Runner
  ( runButton
  , pressButton
  , releaseButton
  )
where

import KMonad.Prelude

import KMonad.Core
import KMonad.Domain.Action

-- | The environment in which 'Button' actions are run
data Button = Button
  { _binding       :: !Keycode             -- ^ The 'Keycode' to which the button is bound
  , _pressAction   :: !Action              -- ^ Action to take when pressed
  , _releaseAction :: !Action              -- ^ Action to take when released
  , _lastAction    :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
  }
makeClassy ''Button

-- newtype Button = Button { unButton :: (SwitchAction -> IO ())}


-- | Perform a 'SwitchAction' in a 'ButtonEnv'
--
-- If the last action was a 'Release' and this action is a 'Press', then run the
-- 'pressAction'. If the last action was a 'Press' and this action is a
-- 'Release', then run the 'releaseAction', else run no action.
runButton :: SwitchAction -> Button -> RIO e ()
runButton current b = do
  last  <- takeMVar $ b^.lastAction
  case (current, last) of
    (Press, Release) -> runAction $ b^.pressAction   -- Press after release
    (Release, Press) -> runAction $ b^.releaseAction -- Release after press
    _                -> pure ()                      -- P&P or R&R
  putMVar (b^.lastAction) current

-- | Send a 'Press' signal to the 'Button'
pressButton :: Button -> RIO e ()
pressButton = runButton Press

-- | Send a 'Release' signal to the 'Button'
releaseButton :: Button -> RIO e ()
releaseButton = runButton Release
