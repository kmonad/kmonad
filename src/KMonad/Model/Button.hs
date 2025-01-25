{-|
Module      : KMonad.Model.Button
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
module KMonad.Model.Button
  ( -- * Button basics
    -- $but
    Button
  , HasButton(..)
  , onPress
  , onRelease
  , onTap
  , mkButton
  , around
  , tapOn

  -- * Simple buttons
  -- $simple
  , emitB
  , pressOnly
  , releaseOnly
  , modded
  , layerToggle
  , layerSwitch
  , layerAdd
  , layerRem
  , pass
  , cmdButton

  -- * Button combinators
  -- $combinators
  , aroundOnly
  , aroundWhenAlone
  , aroundNext
  , aroundNextTimeout
  , aroundNextSingle
  , beforeAfterNext
  , layerDelay
  , layerNext
  , tapHold
  , multiTap
  , tapNext
  , tapHoldNext
  , tapNextRelease
  , tapHoldNextRelease
  , tapNextPress
  , tapMacro
  , tapMacroRelease
  , steppedButton
  , stickyKey
  )
where

import KMonad.Prelude

import KMonad.Model.Action
import KMonad.Keyboard
import KMonad.Util

import qualified RIO.HashSet as S

--------------------------------------------------------------------------------
-- $but
--
-- This section contains the basic definition of KMonad's 'Button' datatype. A
-- 'Button' is essentially a collection of 2 different actions, 1 to perform on
-- 'Press' and another on 'Release'.

-- | A 'Button' consists of three 'MonadK' actions, one to take when a press is
-- registered from the OS, and another when a release is registered.
-- With a third when both should happen in sequence. This will only be used
-- by other button such as `tap-macro`.
-- Use 'mkButton' instead of this constructor.
data Button = Button
  { _pressAction   :: !Action -- ^ Action to take when pressed
  , _releaseAction :: !Action -- ^ Action to take when released
  , _tapAction     :: !Action -- ^ Action to take when tapped as a sub button
  }
makeClassy ''Button

-- | Create a 'Button' out of a press and release action
--
-- NOTE: Since 'AnyK' is an existentially qualified 'MonadK', the monadic
-- actions specified must be runnable by all implementations of 'MonadK', and
-- therefore can only rely on functionality from 'MonadK'. I.e. the actions must
-- be pure 'MonadK'.
mkButton :: AnyK () -> AnyK () -> Button
mkButton a b = mkButton' a b $ a *> b

-- | Create a 'Button' out of a press, release action and tap action
--
-- The non standard tap action is useful when inside other buttons
-- like `tap-macro`
mkButton' :: AnyK () -> AnyK () -> AnyK () -> Button
mkButton' a b c = Button (Action a) (Action b) (Action c)

-- | Create a new button with only a 'Press' action
onPress :: AnyK () -> Button
onPress p = mkButton p $ pure ()

onRelease :: AnyK () -> Button
onRelease = mkButton (pure ())

onTap :: AnyK () -> Button
onTap = mkButton' (pure ()) (pure ())

-- | Like 'onPress' but with an alternative button to use for tapping
onPress' :: Button -> AnyK () -> Button
onPress' (Button{_tapAction = Action t}) p = mkButton' p (pure ()) t

--------------------------------------------------------------------------------
-- $running
--
-- Triggering the actions stored in a 'Button'.

-- | Perform both the press and release of a button immediately
tap :: MonadK m => Button -> m ()
tap b = runAction $ b^.tapAction

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

-- | A button that emits only a Press of a keycode.
pressOnly :: Keycode -> Button
pressOnly c = onPress $ emit $ mkPress c

-- | A button that emits only a Release of a keycode.
releaseOnly :: Keycode -> Button
releaseOnly c = onPress $ emit $ mkRelease c

-- | Create a new button that first presses a 'Keycode' before running an inner
-- button, releasing the 'Keycode' again after the inner 'Button' is released.
modded ::
     Keycode -- ^ The 'Keycode' to `wrap around` the inner button
  -> Button  -- ^ The button to nest inside `being modded`
  -> Button
modded modder = around (emitB modder)

-- | Create a button that toggles a layer on and off
layerToggle :: LayerTag -> Button
layerToggle t = mkButton'
  (layerOp $ PushLayer t)
  (layerOp $ PopLayer  t)
  (pure ())

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

-- | Create a button that executes a shell command on press and possibly on
-- release
cmdButton :: Text -> Maybe Text -> Button
cmdButton pr mbR = mkButton (shellCmd pr) (maybe (pure ()) shellCmd mbR)

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
around outer inner = mkButton
  (runAction (outer^.pressAction)   *> runAction (inner^.pressAction))
  (runAction (inner^.releaseAction) *> runAction (outer^.releaseAction))

-- | A variant of `around`, which releases its outer button when another key
-- is pressed.
aroundOnly ::
     Button -- ^ The outer 'Button'
  -> Button -- ^ The inner 'Button'
  -> Button -- ^ The resulting nested 'Button'
aroundOnly outer inner = onPress' (around outer inner) $ do
  runAction $ outer^.pressAction
  runAction $ inner^.pressAction
  go =<< matchMy Release
 where
  go :: KeyPred -> AnyK ()
  go isMyRelease = hookF InputHook $ \e ->
    if
      | isMyRelease e -> do
        runAction $ inner^.releaseAction
        runAction $ outer^.releaseAction
        pure Catch
      -- Another key is pressed, so release the modifier immediately.
      | isPress e -> do
        runAction $ outer^.releaseAction
        await isMyRelease $ \_ -> do
          runAction (inner^.releaseAction)
          pure Catch
        pure NoCatch
      | otherwise ->
        go isMyRelease $> NoCatch

-- | A variant of `around-only` that represses its outer button when all other
-- keys after it have been released.
aroundWhenAlone ::
     Button -- ^ The outer 'Button'
  -> Button -- ^ The inner 'Button'
  -> Button -- ^ The resulting nested 'Button'
aroundWhenAlone outer inner = onPress' (around outer inner) $ do
  runAction $ outer^.pressAction
  runAction $ inner^.pressAction
  go S.empty =<< matchMy Release
  pure ()
 where
  go :: HashSet Keycode -> KeyPred -> AnyK ()
  go pressed isMyRelease = hookF InputHook $ \e ->
    if
      | isMyRelease e -> do
        runAction $ inner^.releaseAction
        when (null pressed) . runAction $ outer^.releaseAction
        pure Catch
      | isPress e -> do
        let pressed' = S.insert (e^.keycode) pressed
        when (null pressed) . runAction $ outer^.releaseAction
        go pressed' isMyRelease
        pure NoCatch
      | otherwise -> do -- some release
        let pressed' = S.delete (e^.keycode) pressed
        let shouldPressOuter = S.member (e^.keycode) pressed && null pressed'
        inject e
        when shouldPressOuter . after 3 . runAction $ outer^.pressAction
        await isRelease $ \_ -> go pressed' isMyRelease $> NoCatch
        pure Catch

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

-- | A 'Button' that, once pressed, will surround the next button within some timeout with another.
--
-- If some other key is not pressed within an interval another button will be triggered as a tap.
aroundNextTimeout ::
     Milliseconds -- ^ How long before we tap
  -> Button       -- ^ The 'Button' to use to surround next
  -> Button       -- ^ The 'Button' to tap on timeout
  -> Button       -- ^ The resulting button
aroundNextTimeout d b t = onPress $ within d (pure isPress) (tap t) $ \trig -> do
  runAction $ b^.pressAction
  await (isReleaseOf $ trig^.event.keycode) $ \_ -> do
    runAction $ b^.releaseAction
    pure NoCatch
  pure NoCatch

-- | A 'Button' that, once pressed, will surround the next button with another.
--
-- Think of this as, essentially, a tappable mod. For example, an 'aroundNext
-- KeyCtrl' would, once tapped, then make the next keypress C-<whatever>.
--
-- This differs from 'aroundNext' in that it explicitly releases the modifier
-- immediately after the first event, where `aroundSingle` waits around for the
-- original key that was modified to be released itself.
aroundNextSingle ::
     Button -- ^ The outer 'Button'
  -> Button -- ^ The resulting 'Button'
aroundNextSingle b = onPress $ await isPress $ \_ -> do
  runAction $ b^.pressAction
  -- Wait for the next *event*, regardless of what it is
  await (pure True) $ \_ -> do
    runAction $ b^.releaseAction
    pure NoCatch
  pure NoCatch

-- | Create a new button that performs both a press and release of the input
-- button on just a press or release
tapOn ::
     Switch -- ^ Which 'Switch' should trigger the tap
  -> Button -- ^ The 'Button' to tap
  -> Button -- ^ The tapping 'Button'
tapOn Press   b = onPress $ tap b
tapOn Release b = onRelease $ tap b

-- | Create a 'Button' that performs a tap of one button if it is released
-- within an interval. If the interval is exceeded, press the other button (and
-- release it when a release is detected).
tapHold :: Milliseconds -> Button -> Button -> Button
tapHold ms t h = onPress' t $ withinHeld ms (matchMy Release)
  (press h)                     -- If we catch timeout before release
  (const $ tap t $> Catch) -- If we catch release before timeout

-- | Create a 'Button' that performs a tap of 1 button if the next event is its
-- own release, or else switches to holding some other button if the next event
-- is a different keypress.
tapNext :: Button -> Button -> Button
tapNext t h = onPress' t $ hookF InputHook $ \e -> do
  p <- matchMy Release
  if p e
    then tap t   $> Catch
    else press h $> NoCatch

-- | Like 'tapNext', except that after some interval it switches anyways
tapHoldNext :: Milliseconds -> Button -> Button -> Maybe Button -> Button
tapHoldNext ms t h mtb = onPress $ within ms (pure $ const True) onTimeout $ \tr -> do
  p <- matchMy Release
  if p $ tr^.event
    then tap t   $> Catch
    else press h $> NoCatch
  where
    onTimeout :: MonadK m =>  m ()
    onTimeout = press $ fromMaybe h mtb

-- | Surround some future button with a before and after tap
beforeAfterNext :: Button -> Button -> Button
beforeAfterNext b a = onPress $ do
  tap b
  await isPress $ \e -> do
    await (isReleaseOf $ e^.keycode) $ \_ -> do
      tap a
      pure NoCatch
    pure NoCatch


-- | Create a tap-hold style button that makes its decision based on the next
-- detected release in the following manner:
-- 1. It is the release of this button: We are tapping
-- 2. It is of some other button that was pressed *before* this one, ignore.
-- 3. It is of some other button that was pressed *after* this one, we hold.
--
-- It does all of this while holding processing of other buttons, so time will
-- get rolled back like a TapHold button.
tapNextRelease :: Button -> Button -> Button
tapNextRelease t h = onPress' t $ do
  hold True
  go []
  where
    go :: MonadK m => [Keycode] ->  m ()
    go ks = hookF InputHook $ \e -> do
      p <- matchMy Release
      let isRel = isRelease e
      if
        -- If the next event is my own release: we act as if we were tapped
        | p e -> doTap
        -- If the next event is the release of some button that was held after me
        -- we act as if we were held
        | isRel && (e^.keycode `elem` ks) -> doHold e
        -- Else, if it is a press, store the keycode and wait again
        | not isRel                       -> go ((e^.keycode):ks) $> NoCatch
        -- Else, if it is a release of some button held before me, just ignore
        | otherwise                       -> go ks $> NoCatch

    -- Behave like a tap is simple: tap the button `t` and release processing
    doTap :: MonadK m => m Catch
    doTap = tap t *> hold False $> Catch

    -- Behave like a hold is not simple: first we release the processing hold,
    -- then we catch the release of ButtonX that triggered this action, and then
    -- we rethrow this release.
    doHold :: MonadK m => KeyEvent -> m Catch
    doHold e = press h *> hold False *> inject e $> Catch



-- | Create a tap-hold style button that makes its decision based on the next
-- detected release in the following manner:
-- 1. It is the release of this button: We are tapping
-- 2. It is of some other button that was pressed *before* this one, ignore.
-- 3. It is of some other button that was pressed *after* this one, we hold.
--
-- If we encounter the timeout before any other release, we switch to the
-- specified timeout button, or to the hold button if none is specified.
--
-- It does all of this while holding processing of other buttons, so time will
-- get rolled back like a TapHold button.
tapHoldNextRelease :: Milliseconds -> Button -> Button -> Maybe Button -> Button
tapHoldNextRelease ms t h mtb = onPress' t $ do
  hold True
  go ms []
  where

    go :: MonadK m => Milliseconds -> [Keycode] ->  m ()
    go ms' ks = tHookF InputHook ms' onTimeout $ \r -> do
      p <- matchMy Release
      let e = r^.event
      let isRel = isRelease e
      if
        -- If the next event is my own release: act like tapped
        | p e -> onRelSelf
        -- If the next event is another release that was pressed after me
        | isRel && (e^.keycode `elem` ks) -> onRelOther e
        -- If the next event is a press, store and recurse
        | not isRel -> go (ms' - r^.elapsed) (e^.keycode : ks) $> NoCatch
        -- If the next event is a release of some button pressed before me, recurse
        | otherwise -> go (ms' - r^.elapsed) ks $> NoCatch

    onTimeout :: MonadK m =>  m ()
    onTimeout = press (fromMaybe h mtb) *> hold False

    onRelSelf :: MonadK m => m Catch
    onRelSelf = tap t *> hold False $> Catch

    onRelOther :: MonadK m => KeyEvent -> m Catch
    onRelOther e = press h *> hold False *> inject e $> Catch

-- | Create a button just like tap-release, but also trigger a hold on presses:
-- 1. It is the release of this button: We are tapping
-- 2. It is the press of some other button, we hold
-- 3. It is the release of some other button, ignore.
tapNextPress :: Button -> Button -> Button
tapNextPress t h = onPress' t go
  where
    go :: MonadK m => m ()
    go = hookF InputHook $ \e -> do
      p <- matchMy Release
      if
        -- If the next event is my own release: we act as if we were tapped
        | p e -> doTap
        -- If the next event is a press: we act as if we were held
        | isPress e -> doHold e
        -- Else, if it is a release of some other button, just ignore
        | otherwise -> go $> NoCatch

    -- Behave like a tap
    doTap :: MonadK m => m Catch
    doTap = tap t $> Catch

    -- Behave like a hold:
    -- We catch the event of ButtonX that triggered this action, and then
    -- we rethrow this event after holding.
    doHold :: MonadK m => KeyEvent -> m Catch
    doHold e = press h *> inject e $> Catch

-- | Create a 'Button' that contains a number of delays and 'Button's. As long
-- as the next press is registered before the timeout, the multiTap descends
-- into its list. The moment a delay is exceeded or immediately upon reaching
-- the last button, that button is pressed.
multiTap :: Button -> [(Milliseconds, Button)] -> Button
multiTap l bs = onPress' tap' $ hold True *> go bs
  where
    tap' = case bs of
      []           -> l
      ((_, b) : _) -> b

    go :: [(Milliseconds, Button)] -> AnyK ()
    go []            = press l *> hold False
    go ((ms, b):bs') = do
      -- This is a bit complicated. What we do is:
      -- 1.  We wait for an event
      -- 2A. If it doesn't occur in the interval we press the button from the
      --     list and we are done.
      -- 2B. If we do detect the release of the key that triggered this action,
      --     we must now keep waiting to detect another press.
      -- 2C. If we detect another (unrelated) press event we cancel the
      --     remaining of the multi-tap sequence and trigger a hold on the
      --     current button of the sequence.
      -- 3A. After 2B, if we do not detect a press before the interval is up,
      --     we know a tap occurred, so we tap the current button and we are
      --     done.
      -- 3B. If we detect another press of the same key, then the user is
      --     descending into the buttons tied to this multi-tap, so we recurse
      --     on the remaining buttons.
      -- 3C. If we detect any other (unrelated) press event, then the multi-tap
      --     sequence is cancelled like in 2C. We trigger a tap of the current
      --     button of the sequence.
      let doNext pred onTimeout next ms = tHookF InputHook ms onTimeout $ \t -> do
            pr <- pred
            if | pr (t^.event)      -> next (ms - t^.elapsed) $> Catch
               | isPress (t^.event) -> onTimeout              $> NoCatch
               | otherwise          -> hold False             $> NoCatch
      doNext (matchMy Release)
             (press b *> hold False)
             (doNext (matchMy Press) (tap b *> hold False) (\_ -> go bs'))
             ms

-- | Create a 'Button' that performs a series of taps on press. Note that the
-- last button is only released when the tapMacro itself is released.
tapMacro :: [Button] -> Button
tapMacro bs = mkButton' (go False bs) (pure ()) (go True bs)
  where
    go _ []      = pure ()
    go False [b]     = press b
    go True [b] = tap b
    go forceTap (b:rst) = tap b >> go forceTap rst

-- | Create a 'Button' that performs a series of taps on press,
-- except for the last Button, which is tapped on release.
tapMacroRelease :: [Button] -> Button
tapMacroRelease bs = mkButton' (go False bs) (pure ()) (go True bs)
  where
    go _ []      = pure ()
    go False [b]     = awaitMy Release $ tap b >> pure Catch
    go True [b] = tap b
    go forceTap (b:rst) = tap b >> go forceTap rst

-- | Switch to a layer for a period of time, then automatically switch back
layerDelay :: Milliseconds -> LayerTag -> Button
layerDelay d t = onPress $ do
  layerOp (PushLayer t)
  after d (layerOp $ PopLayer t)

-- | Switch to a layer for the next button-press and switch back automaically.
--
-- NOTE: liable to change, this is essentially just `aroundNext` and
-- `layerToggle` combined.
layerNext :: LayerTag -> Button
layerNext t = onPress $ do
  layerOp (PushLayer t)
  await isPress (\_ -> whenDone (layerOp $ PopLayer t) $> NoCatch)

-- | Make a button into a sticky-key, i.e. a key that acts like it is
-- pressed for the button after it if that button was pressed in the
-- given timeframe.
stickyKey :: Milliseconds -> Button -> Button
stickyKey ms b = onPress go
 where
  go :: MonadK m => m ()
  go = hookF InputHook $ \e -> do
    p <- matchMy Release
    if | p e               -> doTap    $> Catch
         -- My own release; we act as if we were tapped
       | not (isRelease e) -> doHold e $> Catch
         -- The press of another button; act like we are held down
       | otherwise         -> go       $> NoCatch
         -- The release of some other button; ignore these

  doHold :: MonadK m => KeyEvent -> m ()
  doHold e = press b *> inject e

  doTap :: MonadK m => m ()
  doTap =
    within ms
           (pure isPress)  -- presses definitely happen after us
           (pure ())
           (\t -> runAction (b^.pressAction)
               *> inject (t^.event)
               *> after 3 (runAction $ b^.releaseAction)
               $> Catch)

-- | Create a button that functions as a different button everything it is pushed
--
-- I.e: first it acts as the first button, then as the second, then as the
-- third, and when finished rotates back to being the first button.
steppedButton :: [Button] -> Button
steppedButton bs = onPress $ go bs
  where
    go [] = undefined
    go [b] = press b
    go (b:bs') = do
      press b
      awaitMy Press $ go bs' $> Catch
