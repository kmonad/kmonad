module KMonad.Button
  -- ( onPress

  -- , runButton
  -- , pressButton
  -- , releaseButton

  -- , around
  -- , tapOn

  -- , tapHold
  -- , multiTap
  -- , tapNext

  -- , module KMonad.Button.Keymap
  -- , module KMonad.Button.Types
  -- )
where

import Prelude

import KMonad.Button.Action
-- import KMonad.Button.Types
import KMonad.Keyboard
import KMonad.Util

import qualified RIO.NonEmpty as N

--------------------------------------------------------------------------------
-- $button
--


-- | The configurable aspects of a 'Button'
data Button = Button
  { _pressAction   :: !(Action ()) -- ^ Action to take when pressed
  , _releaseAction :: !(Action ()) -- ^ Action to take when released
  }
makeClassy ''Button

-- | Create a 'Button' out of a press and release action
mkButton :: MB () -> MB () -> Button
mkButton a b = Button (mkAction a) (mkAction b)

-- | Create a new button with only a 'Press' action
onPress :: MB () -> Button
onPress p = mkButton p (pure ())


-- | The configuration of a 'Button' with some additional state to keep track of
-- the last 'SwitchAction'
data ButtonEnv = ButtonEnv
  { _beButton   :: !Button              -- ^ The configuration for this button
  , _binding    :: !Keycode             -- ^ The 'Keycode' to which this button is bound
  , _lastAction :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
  }
makeClassy ''ButtonEnv

-- | Initialize a 'Button' from a 'Button' and a binding
mkButtonEnv :: Button -> Keycode -> RIO e ButtonEnv
mkButtonEnv b c = ButtonEnv b c <$> newMVar Release

instance HasButton ButtonEnv where button = beButton




-- -- | Run a 'Button' on a 'SwitchAction' returning an 'Action' to be performed by
-- -- the engine. This will only do something if a 'Press' is followed a 'Release'
-- -- or vice-versa, pressing or releasing more than once in sequence does nothing.
-- runButton :: SwitchAction -> ButtonEnv -> RIO e Action
-- runButton a b = do
--   modifyMVar (b^.lastAction) $ \l -> pure $ case (a, l) of
--     (Press, Release) -> (Press,   b^.pressAction)
--     (Release, Press) -> (Release, b^.releaseAction)
--     _                -> (a,       pure ())

-- -- | Press the 'Button'
-- pressButton :: ButtonEnv -> RIO e Action
-- pressButton = runButton Press

-- -- | Release the 'Button'
-- releaseButton :: ButtonEnv -> RIO e Action
-- releaseButton = runButton Release



--------------------------------------------------------------------------------
-- $running

-- | Perform both the press and release of a button immediately
tap :: MonadButton m => Button -> m ()
tap b = do
  runAction $ b^.pressAction
  runAction $ b^.releaseAction

-- | Perform the press action of a Button and register its release callback
press :: MonadButton m => Button -> m ()
press b = do
  runAction $ b^.pressAction
  onRelease (const . runAction $ b^.releaseAction :: KeyEvent -> MB ())


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
  (outer^.pressAction   *> inner^.pressAction)
  (inner^.releaseAction *> outer^.releaseAction)

-- | Create a new button that performs both a press and release of the input
-- button on just a press or release
tapOn ::
     SwitchAction -- ^ Which 'SwitchAction' should trigger the tap
  -> Button       -- ^ The 'Button' to tap
  -> Button       -- ^ The tapping 'Button'
tapOn Press   b = mkButton (tap b)   (pure ())
tapOn Release b = mkButton (pure ()) (tap b)

-- | Create a 'Button' that performs a tap of one button if it is released
-- within an interval. If the interval is exceeded, press the other button (and
-- release it when a release is detected).
tapHold :: Milliseconds -> Button -> Button -> Button
tapHold ms t h = onPress $ catchWithinHeld ms (matchMy Release) $ \case
    Match _ -> tap t
    NoMatch -> press h

-- | Create a 'Button' that contains a number of delays and 'Button's. As long
-- as the next press is registered before the timeout, the multiTap descends
-- into its list. The moment a delay is exceeded or immediately upon reaching
-- the last button, that button is pressed.
multiTap :: (Button, [(Milliseconds, Button)]) -> Button
multiTap bs' = onPress $ go bs'
  where
    go :: (Button, [(Milliseconds, Button)]) -> MB ()
    go (l, []) = press l
    go (l, (ms, b):bs) = catchWithinHeld ms (matchMy Press) $ \case
        Match _ -> go (l, bs)
        NoMatch -> press b

-- | Create a 'Button' that performs a tap of one button if the next event is
-- its own release, or else it presses another button (and releases it when a
-- release is detected).
tapNext :: Button -> Button -> Button
tapNext t h = onPress $ catchNext (matchMy Release) $ \case
    Match _ -> tap t
    NoMatch -> press h


--------------------------------------------------------------------------------
-- $simple
--
-- A collection of simple buttons

-- | A button that emits a Press of a keycode when pressed, and a release when
-- released.
emitB :: Keycode -> Button
emitB c = mkButton
  (emit $ keyPress c)
  (emit $ keyRelease c)

-- | Create a new button first presses a 'Keycode' before running an inner
-- button, releasing the 'Keycode' again after the inner 'Button' is released.
modded ::
     Keycode -- ^ The 'Keycode' to `wrap around` the inner button
  -> Button  -- ^ The button to nest inside `being modded`
  -> Button
modded modder = around (emitB modder)
