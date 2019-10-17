{-|
Module      : KMonad.Domain.Button.Buttons.TapNext
Description : A button that behaves differently depending on the next keypress
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

The TapNext button is another way of specifying a button that behaves
differently when tapped or help (another example being the TapHold). Where a
TapHold is based around a timer, the TapNext only considers the next keypress.
When the next keypress is a release of the same key, we register one key, when
it is something else, we behave as if we are being held.

-}
module KMonad.Domain.Button.Buttons.TapNext
  ( mkTapNext
  )
where

import Control.Lens
import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Domain.Button.Button

-- | Return a new TapNext button
mkTapNext :: (MonadIO io, CanButton m)
  => KeyCode   -- ^ The keycode that triggers this TapNext
  -> Button m  -- ^ The button to use for taps
  -> Button m  -- ^ The button to use for holds
  -> io (Button m)
mkTapNext c t h = mkButton $ \case
  Disengaged -> release h    -- Release the hold button, does nothing if hold was never pressed
  Engaged    -> do
    hold True
    nxt <- waitNext
    let f = nxt >>= \e -> if
          -- If next event is another press (by definition another button), start hold
          | e^.switchState == Engaged                         -> press h
          -- If next event is the release of the TapNext button, tap
          | e^.switchState == Disengaged && (e^.keyCode == c) -> tap t
          -- Otherwise loop
          | otherwise               -> f
    fork (f >> hold False)
