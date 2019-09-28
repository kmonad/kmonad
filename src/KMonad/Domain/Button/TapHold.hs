{-|
Module      : KMonad.Domain.Button.TapHold
Description : A button that behaves differently when tapped or held
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

A TapHold button manages two different buttons, one that is triggered when the
TapHold is pressed and released within its time window, and another that is
triggered when the TapHold is held longer.

When waiting to decide what action to take, a TapHold puts a hold on further
processing. The moment a decision is made, it releases the hold. Let's say we
have a TapHold, th, that taps an A, but holds a LeftShift, and the delay is 1
second, then:

>>> Press th, Press b, Press c, Release th before 1 second, Release b, Release c
Results in: Tap a, Tap b, Tap c

>>> Press th, Press b, Press c, 1 second passes, Release b, Release c
Results in: Press Shift, Tap b, Tap c. When th is released, so will be the Shift.

-}
module KMonad.Domain.Button.TapHold
  ( mkTapHold
  )
where

import Control.Lens
import Control.Monad.Reader

import KMonad.Core
import KMonad.Domain.Effect

--------------------------------------------------------------------------------
-- Setup the running environment and Monad for TapMod buttons

-- | The state of the button
data BState
  = Unpressed
  | Pressed
  deriving (Eq, Show)


-- | The runtime environment of a TapMod
data THEnv m = THEnv
  { _delay     :: Microseconds -- ^ Delay between pushing and switch to held, in us
  , _tapB      :: Button m     -- ^ Handler to use for tap events
  , _holdB     :: Button m     -- ^ Handler to use for hold events
  , _bState    :: Var BState   -- ^ The current phase of the button
  }
makeClassy ''THEnv

type TH m = ReaderT (THEnv m) m

putS :: MonadVar m => BState -> TH m ()
putS st = lift . putVar st =<< view bState

getS :: MonadVar m => TH m BState
getS = lift . getVar =<< view bState


-- | Return a new TapHold button
mkTapHold :: (CanButton m, MonadIO n)
  => Milliseconds -- ^ The time to 'wait' for the next event in milliseconds
  -> Button m     -- ^ The button to use when tapped
  -> Button m     -- ^ The button to use when held
  -> n (Button m)
mkTapHold d t h = runVar Unpressed $ \st -> do
  let env = THEnv { _delay  = fromIntegral $ d * 1000
                  , _tapB   = t
                  , _holdB  = h
                  , _bState = st
                  }
  mkButton $ \x -> runReaderT (go x) env

-- | Process a 'ButtonSignal'
go :: CanButton m => ButtonSignal -> TH m ()
go x = getS >>= \bs ->
  case (bs, x) of
    (Unpressed, BPress) -> putS Pressed   >> hPress
    (Pressed, BRelease) -> putS Unpressed >> hRelease
    _                   -> putS bs

-- | Start monitoring the future to decide what to do
hPress :: CanButton m => TH m ()
hPress = ask >>= \env -> lift  $ do
  hold True
  fork $ do
    race (wait $ env^.delay) runMonitor >>= \case
      Left _  -> bPress $ env^.holdB -- If we outwaited the delay
      Right _ -> do                  -- If we encountered a release first
        bTap $ env^.tapB
        void . swapVar Unpressed $ env^.bState
    hold False

-- | Pin to the current event and return () if any future event shares the same keycode
runMonitor :: CanButton m => m ()
runMonitor = do
  nxt <- pinComparison
  let f = nxt >>= \cmp -> do
        if cmp^.sameCode
          then return ()
          else f
  f

-- | Release the holdB button
hRelease :: CanButton m => TH m ()
hRelease = view holdB >>= lift . bRelease

