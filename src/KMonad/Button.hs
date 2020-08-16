{-|
Module      : KMonad.Button
Description : How buttons work
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

A button contains 2 actions, one to perform on press, and another to perform on
release. This module contains that definition, and some helper code that helps
combine buttons. It is here that most of the complicated` buttons are
implemented (like TapHold).

-}
module KMonad.Button
  ( -- * Button basics
    -- $but
    Button
  , HasButton(..)
  , onPress
  , mkButton
  , around
  , tapOn

  -- * Simple buttons
  -- $simple
  , emitB
  , modded
  , layerToggle
  , layerSwitch
  , layerAdd
  , layerRem
  , pass

  -- * Button combinators
  -- $combinators
  , aroundNext
  , layerDelay
  , layerNext
  , tapHold
  , multiTap
  , tapNext
  , tapHoldNext
  , tapNextRelease
  , tapMacro
  )
where

import KMonad.Prelude

import KMonad.Action
import KMonad.Keyboard
import KMonad.Util


--------------------------------------------------------------------------------
-- $but
--
-- This section contains the basic definition of KMonad's 'Button' datatype. A
-- 'Button' is essentially a collection of 2 different actions, 1 to perform on
-- 'Press' and another on 'Release'.

-- | A 'Button' consists of two 'MonadK' actions, one to take when a press is
-- registered from the OS, and another when a release is registered.
data Button = Button
  { _pressAction   :: !Action -- ^ Action to take when pressed
  , _releaseAction :: !Action -- ^ Action to take when released
  }
makeClassy ''Button

-- | Create a 'Button' out of a press and release action
--
-- NOTE: Since 'AnyK' is an existentially qualified 'MonadK', the monadic
-- actions specified must be runnable by all implementations of 'MonadK', and
-- therefore can only rely on functionality from 'MonadK'. I.e. the actions must
-- be pure 'MonadK'.
mkButton :: AnyK () -> AnyK () -> Button
mkButton a b = Button (Action a) (Action b)

-- | Create a new button with only a 'Press' action
onPress :: AnyK () -> Button
onPress p = mkButton p $ pure ()


--------------------------------------------------------------------------------
-- $running
--
-- Triggering the actions stored in a 'Button'.

-- | Perform both the press and release of a button immediately
tap :: MonadK m => Button -> m ()
tap b = do
  runAction $ b^.pressAction
  runAction $ b^.releaseAction

-- | Perform the press action of a Button and register its release callback.
--
-- This performs the action stored in the 'pressAction' field and registers a
-- callback that will trigger the 'releaseAction' when the release is detected.
press :: MonadK m => Button -> m ()
press b = do
  runAction $ b^.pressAction
  awaitMy Release $ do
    runAction $ b^.releaseAction
    pure Catch

--------------------------------------------------------------------------------
-- $simple
--
-- A collection of simple buttons. These are basically almost direct wrappings
-- around 'MonadK' functionality.

-- | A button that emits a Press of a keycode when pressed, and a release when
-- released.
emitB :: Keycode -> Button
emitB c = mkButton
  (emit $ mkPress c)
  (emit $ mkRelease c)

-- | Create a new button that first presses a 'Keycode' before running an inner
-- button, releasing the 'Keycode' again after the inner 'Button' is released.
modded ::
     Keycode -- ^ The 'Keycode' to `wrap around` the inner button
  -> Button  -- ^ The button to nest inside `being modded`
  -> Button
modded modder = around (emitB modder)

-- | Create a button that toggles a layer on and off
layerToggle :: LayerTag -> Button
layerToggle t = mkButton
  (layerOp $ PushLayer t)
  (layerOp $ PopLayer  t)

-- | Create a button that switches the base-layer on a press
layerSwitch :: LayerTag -> Button
layerSwitch t = onPress (layerOp $ SetBaseLayer t)

-- | Create a button that adds a layer on a press
layerAdd :: LayerTag -> Button
layerAdd t = onPress (layerOp $ PushLayer t)

-- | Create a button that removes the top instance of a layer on a press
layerRem :: LayerTag -> Button
layerRem t = onPress (layerOp $ PopLayer t)

-- | Create a button that does nothing (but captures the input)
pass :: Button
pass = onPress $ pure ()


--------------------------------------------------------------------------------
-- $combinators
--
-- Functions that take 'Button's and combine them to form new 'Button's.

-- | Create a new button from 2 buttons, an inner and an outer. When the new
-- button is pressed, first the outer is pressed, then the inner. On release,
-- the inner is released first, and then the outer.
around ::
     Button -- ^ The outer 'Button'
  -> Button -- ^ The inner 'Button'
  -> Button -- ^ The resulting nested 'Button'
around outer inner = Button
  (Action (runAction (outer^.pressAction)   *> runAction (inner^.pressAction)))
  (Action (runAction (inner^.releaseAction) *> runAction (outer^.releaseAction)))

-- | A 'Button' that, once pressed, will surround the next button with another.
--
-- Think of this as, essentially, a tappable mod. For example, an 'aroundNext
-- KeyCtrl' would, once tapped, then make the next keypress C-<whatever>.
aroundNext ::
     Button -- ^ The outer 'Button'
  -> Button -- ^ The resulting 'Button'
aroundNext b = onPress $ await isPress $ \e -> do
  runAction $ b^.pressAction
  await (isReleaseOf $ e^.keycode) $ \_ -> do
    runAction $ b^.releaseAction
    pure NoCatch
  pure NoCatch

-- | Create a new button that performs both a press and release of the input
-- button on just a press or release
tapOn ::
     Switch -- ^ Which 'Switch' should trigger the tap
  -> Button -- ^ The 'Button' to tap
  -> Button -- ^ The tapping 'Button'
tapOn Press   b = mkButton (tap b)   (pure ())
tapOn Release b = mkButton (pure ()) (tap b)

-- | Create a 'Button' that performs a tap of one button if it is released
-- within an interval. If the interval is exceeded, press the other button (and
-- release it when a release is detected).
tapHold :: Milliseconds -> Button -> Button -> Button
tapHold ms t h = onPress $ withinHeld ms (matchMy Release)
  (press h)                     -- If we catch timeout before release
  (const $ tap t *> pure Catch) -- If we catch release before timeout

-- | Create a 'Button' that performs a tap of 1 button if the next event is its
-- own release, or else switches to holding some other button if the next event
-- is a different keypress.
tapNext :: Button -> Button -> Button
tapNext t h = onPress $ hookF $ \e -> do
  p <- matchMy Release
  if p e
    then tap t   *> pure Catch
    else press h *> pure NoCatch

-- | Like 'tapNext', except that after some interval it switches anyways
tapHoldNext :: Milliseconds -> Button -> Button -> Button
tapHoldNext ms t h = onPress $ within ms (pure $ const True) (press h) $ \tr -> do
  p <- matchMy Release
  if p $ tr^.event
    then tap t   *> pure Catch
    else press h *> pure NoCatch

-- | Create a tap-hold style button that makes its decision based on the next
-- detected release in the following manner:
-- 1. It is the release of this button: We are tapping
-- 2. It is of some other button that was pressed *before* this one, ignore.
-- 3. It is of some other button that was pressed *after* this one, we hold.
--
-- It does all of this while holding processing of other buttons, so time will
-- get rolled back like a TapHold button.
tapNextRelease :: Button -> Button -> Button
tapNextRelease t h = onPress $ do
  hold True
  go []
  where
    go :: MonadK m => [Keycode] ->  m ()
    go ks = hookF $ \e -> do
      p <- matchMy Release
      let isRel = isRelease e
      if
        -- If the next event is my own release: we act as if we were tapped
        | p e -> doTap
        -- If the next event is the release of some button that was held after me
        -- we act as if we were held
        | isRel && (e^.keycode `elem` ks) -> doHold e
        -- Else, if it is a press, store the keycode and wait again
        | not isRel                       -> go ((e^.keycode):ks) *> pure NoCatch
        -- Else, if it is a release of some button held before me, just ignore
        | otherwise                       -> go ks *> pure NoCatch

    -- Behave like a tap is simple: tap the button `t` and release processing
    doTap :: MonadK m => m Catch
    doTap = tap t *> hold False *> pure Catch

    -- Behave like a hold is not simple: first we release the processing hold,
    -- then we catch the release of ButtonX that triggered this action, and then
    -- we rethrow this release.
    doHold :: MonadK m => KeyEvent -> m Catch
    doHold e = press h *> hold False *> inject e *> pure Catch
       
-- | Create a 'Button' that contains a number of delays and 'Button's. As long
-- as the next press is registered before the timeout, the multiTap descends
-- into its list. The moment a delay is exceeded or immediately upon reaching
-- the last button, that button is pressed.
multiTap :: Button -> [(Milliseconds, Button)] -> Button
multiTap l bs = onPress $ go bs
  where
    go :: [(Milliseconds, Button)] -> AnyK ()
    go []            = press l
    go ((ms, b):bs') = do
      -- This is a bit complicated. What we do is:
      -- 1.  We wait for the release of the key that triggered this action
      -- 2A. If it doesn't occur in the interval we press the button from the list
      --     and we are done.
      -- 2B. If we do detect the release, we must now keep waiting to detect another press.
      -- 3A. If we do not detect a press before the interval is up, we know a tap occured,
      --     so we tap the current button and we are done.
      -- 3B. If we detect another press, then the user is descending into the buttons tied
      --     to this multi-tap, so we recurse on the remaining buttons.
      let onMatch t = do
            within (ms - t^.elapsed) (matchMy Press)
                   (tap b)
                   (const $ go bs' *> pure Catch)
            pure Catch
      within ms (matchMy Release) (press b) onMatch


-- | Create a 'Button' that performs a series of taps on press.
tapMacro :: [Button] -> Button
tapMacro bs = onPress $ mapM_ tap bs

-- | Switch to a layer for a period of time, then automatically switch back
layerDelay :: Milliseconds -> LayerTag -> Button
layerDelay d t = onPress $ do
  layerOp (PushLayer t)
  after d (layerOp $ PopLayer t)

layerNext :: LayerTag -> Button
layerNext t = onPress $ do
  layerOp (PushLayer t)
  await isPress (\_ -> whenDone (layerOp $ PopLayer t) *> pure NoCatch)
