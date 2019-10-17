{-|
Module      : KMonad.Domain.Button.Buttons.TapHold
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
module KMonad.Domain.Button.Buttons.TapHold
  ( mkTapHold
  )
where

import Control.Lens
import Control.Monad.Reader

import KMonad.Core
import KMonad.Domain.Effect

--------------------------------------------------------------------------------
-- Setup the running environment and Monad for TapMod buttons

-- | Return a new TapHold button
mkTapHold :: CanButton m
  => KeyCode      -- ^ The keycode that triggers this taphold
  -> Milliseconds -- ^ The time to 'wait' for the next event in milliseconds
  -> Button m     -- ^ The button to use when tapped
  -> Button m     -- ^ The button to use when held
  -> Button m
mkTapHold c d t h = mkButton $ \case
  Disengaged -> release h      -- Release the hold button, does nothing if hold was never pressed
  Engaged    -> do
    hold True
    fork $ do
      race (wait us) (waitFor myRel) >>= \case
        Left  _ -> press h -- If we outwaited the delay
        Right _ -> tap t   -- If we encountered a release first
      hold False
  where
    us      = fromIntegral (d * 1000)
    myRel e = (e^.keyCode == c) && (e^.switchState == Disengaged)
