module KMonad.Types.Button
  ( ButtonCfg(..)
  , Button(..)
  , HasButtonCfg(..)
  , HasButton(..)

  , runButton
  , pressButton
  , releaseButton

  , mkEmit
  )
where

import KMonad.Prelude


import UnliftIO.Async      (race)
import UnliftIO.Concurrent (forkIO)

import KMonad.Types.Action
import KMonad.Types.Keyboard.Event
import KMonad.Types.Keyboard.IO

--------------------------------------------------------------------------------
-- $button
--
-- 'Button's are 2 different 'Action's bound to the press and release events of
-- a particular 'Keycode'. 'Button's are structured so that only
-- state-transitions are effectful (press after press does nothing, release
-- after release does nothing).
--
-- Most button functionality is implemented in the "KMonad.Domain.Button"
-- module, but the type is provided here for circular import reasons.

-- | The configurable aspects of a 'Button'
data ButtonCfg = ButtonCfg
  { _pressAction   :: !Action -- ^ Action to take when pressed
  , _releaseAction :: !Action -- ^ Action to take when released
  }
makeClassy ''ButtonCfg

-- | The environment in which 'Button' actions are run
data Button = Button
  { _beBinding    :: !Keycode             -- ^ The 'Keycode' to which the button is bound
  , _beLastAction :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
  , _beButtonCfg  :: !ButtonCfg           -- ^ The configuration for this button
  }
makeLenses ''Button

class HasButtonCfg e => HasButton e where
  buttonEnv :: Lens' e Button

  binding :: Lens' e Keycode
  binding = buttonEnv . beBinding

  lastAction :: Lens' e (MVar SwitchAction)
  lastAction = buttonEnv . beLastAction

instance HasButtonCfg Button where buttonCfg = beButtonCfg
instance HasButton Button where buttonEnv = id



type CanButton e =
  ( HasKeySink   e
  , HasLogFunc   e
  , HasButton e
  )


-- | Perform a 'SwitchAction' in a 'Button'
--
-- If the last action was a 'Release' and this action is a 'Press', then run the
-- 'pressAction'. If the last action was a 'Press' and this action is a
-- 'Release', then run the 'releaseAction', else run no action.
runButton :: CanButton e => SwitchAction -> RIO e ()
runButton current = do
  lastV <- view lastAction
  last  <- takeMVar lastV
  case (current, last) of
    (Press, Release) -> runAction =<< view pressAction   -- Press after release
    (Release, Press) -> runAction =<< view releaseAction -- Release after press
    _                -> pure ()                          -- P&P or R&R
  putMVar lastV current

-- | Send a 'Press' signal to the 'Button'
pressButton :: CanButton e => RIO e ()
pressButton = runButton Press

-- | Send a 'Release' signal to the 'Button'
releaseButton :: CanButton e => RIO e ()
releaseButton = runButton Release


-- | Handle all the different 'Action's
runAction :: CanButton e => Action -> RIO e ()
runAction (Emit a)   = emitKey a
runAction (Fork a)   = void . forkIO $ runAction a
runAction (Macro as) = traverse_ runAction as
runAction (Race (a1, a2) (b1, b2)) = do
  race (runAction a1) (runAction b1) >>= \case
    Left  _ -> runAction a2
    Right _ -> runAction b2
runAction (Pause ms) = threadDelay $ fromIntegral ms
runAction Pass       = pure ()


--------------------------------------------------------------------------------


mkEmit :: Keycode -> ButtonCfg
mkEmit kc = ButtonCfg (Emit $ pressKey kc) (Emit $ releaseKey kc)
