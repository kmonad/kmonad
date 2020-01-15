module KMonad.Button
  ( onPress
  , Button
  , HasButton(..)
  -- , button
  , around
  , tapOn
  , tapHold
  , multiTap
  , tapNext
  , emitB
  , modded

  , module KMonad.Button.Action
  )
where

import Prelude

import KMonad.Button.Action
-- import KMonad.Button.Types
import KMonad.Keyboard
import KMonad.Util

-- import qualified RIO.NonEmpty as N

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



--------------------------------------------------------------------------------
-- $running

-- | Perform both the press and release of a button immediately
tap :: MonadButton m => Button -> m ()
tap b = do
  runAction $ b^.pressAction
  runAction $ b^.releaseAction

-- | Perform the press action of a Button and register its release callback.
--
-- NOTE: The interaction with 'KMonad.Daemon.pressButton' might seem confusing,
-- since both actions hook an action to the release of the button, however it
-- works like this:
--
-- You use 'press' inside 'MonadButton' definitions, it is used in combinators
-- that combine different buttons into one 'Button'. See for example 'tapHold'.
-- When 'press' is used in such a context, it essentially inserts the action of
-- hooking a release into that 'Button's press action.
--
-- When 'pressButton' is used on a 'tapHold', the 'pressAction' from the
-- 'tapHold' will insert a callback, and at the same time, 'pressButton' will
-- insert a separate callback that first of all calls 'tapHold's 'releaseAction'
-- (which is empty), and secondly resets the
-- 'KMonad.Daemon.KeyHandler.ButtonEnv's 'lastAction' to 'Release'.
--
-- This means that if a 'Button' does not require any complex operations, it can
-- simply store its release action in its 'releaseAction' field, where it will
-- automatically get called, or else actively encode hooking release-actions in
-- its 'pressAction' field if more complicated operations are required.
press :: MonadButton m => Button -> m ()
press b = do
  runAction $ b^.pressAction
  catchRelease_ . runAction $ b^.releaseAction


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
tapHold ms t h = onPress $ catchWithinHeld ms (catchMy Release) $ \case
    Catch _ -> tap t
    _       -> press h

-- | Create a 'Button' that contains a number of delays and 'Button's. As long
-- as the next press is registered before the timeout, the multiTap descends
-- into its list. The moment a delay is exceeded or immediately upon reaching
-- the last button, that button is pressed.
multiTap :: Button -> [(Milliseconds, Button)] -> Button
multiTap l bs = onPress $ go bs
  where
    go :: [(Milliseconds, Button)] -> MB ()
    go []             = press l
    go ((ms, b'):bs') = catchWithinHeld ms (catchMy Press) $ \case
        Catch _ -> go bs'
        _       -> press b'

-- | Create a 'Button' that performs a tap of one button if the next event is
-- its own release, or else it presses another button (and releases it when a
-- release is detected).
tapNext :: Button -> Button -> Button
tapNext t h = onPress $ catchNext (catchMy Release) $ \case
    Catch _ -> tap t
    _       -> press h


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
