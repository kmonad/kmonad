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
combine buttons. It is here that most of the `complicated` buttons are
implemented (like TapHold).

-}
module KMonad.Button
  ( Action
  , runAction
  , onPress
  , Button
  , HasButton(..)
  , mkButton
  , around
  , tapOn

  -- $cplx
  , tapHold
  , multiTap
  , tapNext
  , tapMacro

  -- $smpl
  , emitB
  , modded
  , layerToggle
  , layerSwitch
  , pass

  -- $benv
  , BEnv
  , initBEnv
  , runBEnv
  , HasBEnv(..)
 
  -- $reexport
  , module KMonad.Action
  )
where

import KPrelude

import KMonad.Action
import KMonad.Keyboard
import KMonad.Util


--------------------------------------------------------------------------------
-- $action

-- | Type alias for `any monad that can perform MonadK actions`
type AnyK a = forall m. MonadK m => m a

-- | A newtype wrapper used to construct 'MonadK' actions
newtype Action = Action { runAction :: AnyK ()}


--------------------------------------------------------------------------------
-- $button

-- | A 'Button' consists of two 'MonadK' actions, one to take when a press is
-- registered from the OS, and another when a release is registered.
data Button = Button
  { _pressAction   :: !Action -- ^ Action to take when pressed
  , _releaseAction :: !Action -- ^ Action to take when released
  }
makeClassy ''Button

-- | Create a 'Button' out of a press and release action
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
  catchRelease_ . runAction $ b^.releaseAction

--------------------------------------------------------------------------------
-- $bcomb
--
-- A collection of useful button combinators.

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

-- aroundNext ::
--      Button -- ^ The outer 'Button'
--   -> Button -- ^ The resulting 'Button'
-- aroundNext b = onPress $ await (pure $ matchWith (\e -> e^.switch == Press)) $ \e -> do
--   runAction $ b^.pressAction


  



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
tapHold ms t h = onPress $ catchWithinHeld ms (catchMy Release) $ \m -> if
  | m^.succeeded -> tap t
  | otherwise    -> press h

-- | Create a 'Button' that contains a number of delays and 'Button's. As long
-- as the next press is registered before the timeout, the multiTap descends
-- into its list. The moment a delay is exceeded or immediately upon reaching
-- the last button, that button is pressed.
multiTap :: Button -> [(Milliseconds, Button)] -> Button
multiTap l bs = onPress $ go bs
  where
    go :: [(Milliseconds, Button)] -> AnyK ()
    go []             = press l
    go ((ms, b'):bs') = catchWithinHeld ms (catchMy Release) $ \m -> if
        | m^.succeeded -> catchWithinHeld (ms - m^.elapsed) (catchMy Press) $ \m2 -> if
            | m2^.succeeded -> go bs'
            | otherwise     -> tap b'
        | otherwise -> press b'

-- | Create a 'Button' that performs a tap of one button if the next event is
-- its own release, or else it presses another button (and releases it when a
-- release is detected).
tapNext :: Button -> Button -> Button
tapNext t h = onPress $ catchNext (catchMy Release) $ \m -> if
    | m^.succeeded -> tap t
    | otherwise    -> press h

-- | Create a 'Button' that performs a series of taps on press.
tapMacro :: [Button] -> Button
tapMacro bs = onPress $ mapM_ tap bs


--------------------------------------------------------------------------------
-- $simple
--
-- A collection of simple buttons

-- | A button that emits a Press of a keycode when pressed, and a release when
-- released.
emitB :: Keycode -> Button
emitB c = mkButton
  (emit $ mkPress c)
  (emit $ mkRelease c)

-- | Create a new button first presses a 'Keycode' before running an inner
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


-- | Create a button that does nothing (but captures the input)
pass :: Button
pass = onPress $ pure ()


--------------------------------------------------------------------------------
-- $benv
--
-- When running KMonad, a button also keeps track of what keycode it's bound to,
-- and what its last switch was. This is used to provide the 'myBinding' feature
-- of MonadK, and the invariant that switches always alternate (there is no
-- press-press or release-release).

-- | The configuration of a 'Button' with some additional state to keep track of
-- the last 'Switch'
data BEnv = BEnv
  { _beButton   :: !Button        -- ^ The configuration for this button
  , _binding    :: !Keycode       -- ^ The 'Keycode' to which this button is bound
  , _lastSwitch :: !(MVar Switch) -- ^ State to keep track of last manipulation
  }
makeClassy ''BEnv

instance HasButton BEnv where button = beButton

-- | Initialize a 'BEnv', note that a key is always initialized in an unpressed
-- state.
initBEnv :: MonadIO m => Button -> Keycode -> m BEnv
initBEnv b c = BEnv b c <$> newMVar Release

-- | Try to switch a 'BEnv'. This only does something if the 'Switch' is
-- different from the 'lastSwitch' field. I.e. pressing a pressed button or
-- releasing a released button does nothing.
runBEnv :: MonadUnliftIO m => BEnv -> Switch -> m (Maybe Action)
runBEnv b a =
  modifyMVar (b^.lastSwitch) $ \l -> pure $ case (a, l) of
    (Press, Release) -> (Press,   Just $ b^.pressAction)
    (Release, Press) -> (Release, Just $ b^.releaseAction)
    _                -> (a,       Nothing)
