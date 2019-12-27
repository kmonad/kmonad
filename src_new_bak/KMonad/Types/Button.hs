module KMonad.Types.Button

where

import KMonad.Prelude

import UnliftIO.Concurrent

import KMonad.Types.Keyboard.Event
import KMonad.Types.Script
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

class HasButtonCfg e => HasButton e where
  button :: Lens' e Button

  binding :: Lens' e Keycode
  binding = button . beBinding

  lastAction :: Lens' e (MVar SwitchAction)
  lastAction = button . beLastAction

instance HasButtonCfg Button where buttonCfg = beButtonCfg
instance HasButton    Button where button    = id

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


--------------------------------------------------------------------------------
-- $effects
--
-- To run button actions, 'Button's need access to a set of effects that
-- determine what they can do. The different effects are enumerated here and
-- hooked into the 'ButtonEnv' in which 'Action's are always run.

--------------------------------------------------------------------------------

newtype LaunchScriptFunc = LaunchScriptFunc
  { unLSF :: forall e. ScriptCfg -> RIO e () }


--------------------------------------------------------------------------------

newtype InjectEventFunc = InjectEventFunc
  { unIEF :: forall e a. HasKeyAction a => a -> RIO e () }

--------------------------------------------------------------------------------

newtype RegisterPriorityButtonFunc = RegisterPriorityButtonFunc
  { unRPH :: forall e. PriorityButton -> RIO e () }

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

newtype PauseHandlingFunc = PauseHandlingFunc
  { unPHF :: forall e. Bool -> RIO e () }


--------------------------------------------------------------------------------

newtype EmitKeyFunc = EmitKeyFunc
  { unEKF :: forall e a. HasKeyAction a => a -> RIO e () }

--------------------------------------------------------------------------------

-- | The environment in which 'Button' 'Action's are run
data ButtonEnv = ButtonEnv
  { _beLogFunc                     :: LogFunc
  , _beInjectEventFunc             :: InjectEventFunc
  , _beRegisterPriorityButtonFunc  :: RegisterPriorityButtonFunc
  , _bePauseHandlingFunc           :: PauseHandlingFunc
  , _beEmitKeyFunc                 :: EmitKeyFunc
  , _beLaunchScriptFunc            :: LaunchScriptFunc
  , _beButton                      :: Button
  }
makeLenses ''ButtonEnv

instance HasLogFunc   ButtonEnv where logFuncL  = beLogFunc
instance HasButtonCfg ButtonEnv where buttonCfg = button.buttonCfg
instance HasButton    ButtonEnv where button    = beButton


class (HasButton e, HasLogFunc e) => HasButtonEnv e where
  buttonEnv :: Lens' e ButtonEnv

  injectEventFunc :: Lens' e InjectEventFunc
  injectEventFunc = buttonEnv . beInjectEventFunc

  registerPriorityButtonFunc :: Lens' e RegisterPriorityButtonFunc
  registerPriorityButtonFunc = buttonEnv . beRegisterPriorityButtonFunc

  pauseHandlingFunc :: Lens' e PauseHandlingFunc
  pauseHandlingFunc = buttonEnv . bePauseHandlingFunc

  emitKeyFunc :: Lens' e EmitKeyFunc
  emitKeyFunc = buttonEnv . beEmitKeyFunc

  launchScriptFunc :: Lens' e LaunchScriptFunc
  launchScriptFunc = buttonEnv . beLaunchScriptFunc

instance HasButtonEnv ButtonEnv where
  buttonEnv = id

injectEvent :: (HasButtonEnv e, HasKeyAction a) => a -> RIO e ()
injectEvent a = ($ a) =<< (unIEF <$> view injectEventFunc)

pauseHandling :: (HasButtonEnv e) => Bool -> RIO e ()
pauseHandling b = ($ b) =<< (unPHF <$> view pauseHandlingFunc)

registerPriorityButton :: (HasButtonEnv e)
  => PriorityButton -> RIO e ()
registerPriorityButton cfg =
  ($ cfg) =<< (unRPH <$> view registerPriorityButtonFunc)

launchScript :: HasButtonEnv e => ScriptCfg -> RIO e ()
launchScript cfg = ($ cfg) =<< (unLSF <$> view launchScriptFunc)

emitKey :: (HasButtonEnv e, HasKeyAction a) => a -> RIO e ()
emitKey k = ($ k) =<< (unEKF <$> view emitKeyFunc)


--------------------------------------------------------------------------------
-- $running


-- | Recursively traverse an Action AST
runAction :: HasButtonEnv e => Action -> RIO e ()
runAction (Emit ka)
  = emitKey ka
runAction (Pause ms)
  = threadDelay $ fromIntegral ms
runAction (Hold b)
  = pauseHandling b
runAction (Race (a1, a2) (b1, b2))
  = race (runAction a1) (runAction b1) >>= \case
      Left  _ -> runAction a2
      Right _ -> runAction b2
runAction (Macro as)
  = traverse_ runAction as
runAction (Fork a)
  = void . forkIO $ runAction a
runAction Pass
  = pure ()

runPriorityButton :: HasButtonEnv e => PriorityButton -> RIO e ()
runPriorityButton b = runAction $ b^.action

switchButton :: HasButtonEnv e => SwitchAction -> RIO e ()
switchButton new = do
  lastV <- view lastAction
  last  <- takeMVar lastV
  case (new, last) of
    (Press, Release) -> runAction =<< view pressAction
    (Release, Press) -> runAction =<< view releaseAction
    _                -> pure ()
  putMVar lastV new

pressButton :: HasButtonEnv e => RIO e ()
pressButton = switchButton Press

releaseButton :: HasButtonEnv e => RIO e ()
releaseButton = switchButton Release

-- data Button = Button
--   { _beBinding    :: !Keycode             -- ^ The 'Keycode' to which the button is bound
--   , _beLastAction :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
--   , _beButtonCfg  :: !ButtonCfg           -- ^ The configuration for this button
--   }
-- makeLenses ''Button
