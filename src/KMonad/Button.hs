module KMonad.Button

where

import Prelude

import KMonad.Keyboard
import KMonad.Util

--------------------------------------------------------------------------------
-- $action
--
-- 'Action's describe those operations that can be triggered by button events
-- (presses or releases). Most of KMonad is written in RIO style, but 'Action's
-- are purposefully written in a 'MonadButton' (of which RIO 'DaemonEnv' is an
-- instance) to put a clear constraint on the supported capabilities of button
-- actions.
--
-- This restriction allows us to then expose a programmable interface to the
-- user, allowing users to define their own button actions and extend KMonad
-- locally, without ever having to muss about with the code.
--
-- However much the internal implementation of KMonad might change, the exposed
-- 'MonadButton' interface should be left unmolested.

class Monad m => MonadButton m where
  -- | Emit a KeyAction to the OS
  emit      :: KeyAction -> m ()
  -- | Pause the current thread for n milliseconds
  pause     :: Milliseconds -> m ()
  -- | Race two actions, pass the winner to a function
  race      :: (m a, m b) -> (Either a b -> m c) -> m c
  -- | Pause or unpause event processing
  hold      :: Bool -> m ()
  -- | Fork an action to the background and keep running
  fork      :: m a -> m (Async a)
  -- | Access the keycode to which this button is bound
  myBinding :: m Keycode
  -- | Wait for a 'KeyEvent' that matches a predicate, capturing
  -- it in the process and short-circuiting regular processing
  capture   :: (KeyEvent -> Bool) -> m KeyEvent

-- | An 'Action' is a wrapper around a monadic computation that is guaranteed to
-- use only 'MonadButton' functionality.
newtype Action = Action {unAction :: forall m . MonadButton m => m ()}

-- | Unwrap an action and perform it
runAction :: MonadButton m => Action -> m ()
runAction = unAction

-- | Combine actions by sequencing them
instance Semigroup Action where
  (Action a) <> (Action b) = Action $ a >> b
instance Monoid Action where
  mempty = Action $ pure ()





--------------------------------------------------------------------------------
-- $button
--
-- A button is a collection of 2 actions: 1 to take when pressed, and 1 to take
-- when released. 'Button's are further designed thusly that an effect will only
-- occur if a press follows a release or vice versa. Repeatedly pressing or
-- releasing a button will only trigger the associated action once.
--
--

-- | The configurable aspects of a 'Button'
data Button = Button
  { _pressAction   :: !Action -- ^ Action to take when pressed
  , _releaseAction :: !Action -- ^ Action to take when released
  }
makeClassy ''Button

-- | Create a new button
mkButton ::
     (forall m. MonadButton m => m ())
  -> (forall n. MonadButton n => n ())
  -> Button
mkButton p r = Button (Action p) (Action r)

-- | The unique state identifying 1 'button'
data ButtonEnv = ButtonEnv
  { __binding    :: !Keycode             -- ^ The 'Keycode' to which the button is bound
  , _beButton  :: !Button           -- ^ The configuration for this button
  , _lastAction :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
  }
makeLenses ''ButtonEnv

class HasBinding e where binding :: Lens' e Keycode
instance HasBinding ButtonEnv where binding = _binding


instance HasButton ButtonEnv where button = beButton

-- | Initialize a 'Button' from a 'Button' and a binding
initButtonEnv :: Keycode -> Button -> RIO e ButtonEnv
initButtonEnv kc cfg = ButtonEnv kc cfg <$> newMVar Release

-- | Run a 'Button' on a 'SwitchAction' returning an 'Action' to be performed by
-- the engine. This will only do something if a 'Press' followed a 'Release' or
-- vice-versa, pressing or releasing more than once in sequence does nothing.
runButton :: SwitchAction -> ButtonEnv-> RIO e Action
runButton a b = do
  modifyMVar (b^.lastAction) $ \l -> pure $ case (a, l) of
    (Press, Release) -> (Press,   b^.pressAction)
    (Release, Press) -> (Release, b^.releaseAction)
    _                -> (a,       mempty)


-- | Press the 'Button'
pressButton :: ButtonEnv-> RIO e Action
pressButton = runButton Press

-- | Release the 'Button'
releaseButton :: ButtonEnv-> RIO e Action
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
  -- , _phButton :: Button
  }
makeClassy ''PriorityButton

runPriorityButton :: PriorityButton -> RIO e Action
runPriorityButton = undefined


--------------------------------------------------------------------------------
-- $acomb
--
-- A collection of simple action combinators

-- | Do nothing
pass :: MonadButton m => m ()
pass = pure ()

-- | Try a certain action, and if it succeeds before the timeout is complete,
-- return its result. Otherwise interrupt the action and return Nothing.
within :: MonadButton m => Milliseconds -> m a -> m (Maybe a)
within ms a = race (pause ms, a) $ \case
  Left  _ -> pure $ Nothing
  Right r -> pure $ Just r

-- | Perform both the press and release of a button in sequence
tap :: MonadButton m => Button -> m ()
tap b = press b >> release b

-- | Perform the press action of a Button
press :: MonadButton m => Button -> m ()
press = runAction . view pressAction

-- | Perform the release action of a Button
release :: MonadButton m => Button -> m ()
release = runAction . view releaseAction

-- | Return the 'KeyAction' that corresponds to the 'Release' of the currently
-- processing 'Button'
myRelease :: MonadButton m => m KeyAction
myRelease = keyRelease <$> myBinding

tapHold :: Milliseconds -> Button -> Button -> Button
tapHold ms t h = Button p (Action $ release h)
  where
    p :: Action
    p = Action $ do
      hold True
      void . fork $ do
        released <- (\a e -> a == e^.keyAction) <$> myRelease
        within ms (capture released) >>= \case
          Nothing -> press h
          Just _  -> tap   t
        hold  False


--------------------------------------------------------------------------------
-- $buttons
--
-- A variety of preconfigured buttons and button combinators

-- | A button that emits a Press of a keycode when pressed, and a release when
-- released.
emitB :: Keycode -> Button
emitB c = mkButton
  (emit $ keyPress c)
  (emit $ keyRelease c)


--------------------------------------------------------------------------------
-- $bcomb
--
-- A collection of useful button combinators

-- | Create a new button from 2 buttons, an inner and an outer. When the new
-- button is pressed, first the outer is pressed, then the inner. On release,
-- the inner is released first, and then the outer.
around ::
     Button -- ^ The outer 'Button'
  -> Button -- ^ The inner 'Button'
  -> Button -- ^ The resulting nested 'Button'
around outer inner = Button
  (outer^.pressAction   <> inner^.pressAction)
  (inner^.releaseAction <> outer^.releaseAction)

-- | Create a new button first presses a 'Keycode' before running an inner
-- button, releasing the 'Keycode' again after the inner 'Button' is released.
modded ::
     Keycode
  -> Button
  -> Button
modded kc = around (emitB kc)

-- | Create a new button that performs both a press and release of the input
-- button on just a press or release
tapOn ::
     SwitchAction -- ^ Which 'SwitchAction' should trigger the tap
  -> Button       -- ^ The 'Button' to tap
  -> Button       -- ^ The tapping 'Button'
tapOn s b = let f = if s == Press then Button else flip Button in
  f (b^.pressAction <> b^.releaseAction) mempty
