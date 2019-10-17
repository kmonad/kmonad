{-|
Module      : KMonad.Domain.Button.Buttons.MultiTap
Description : A button that does different things depending on how often it is tapped
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)


-}
module KMonad.Domain.Button.Buttons.MultiTap
  ( mkMultiTap
  )
where

import Control.Lens
import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Domain.Button.Button

-- | Return a button that does changes behavior based on how often it is tapped.
--
-- Each potential
mkMultiTap :: (MonadIO io, CanButton m)
  => KeyCode                    -- ^ The 'KeyCode' to which this button is mapped
  -> [(Microseconds, Button m)] -- ^ A list of (delay, button) tuples
  -> io (Button m)
mkMultiTap c bs = mkButton $ \case
  Disengaged -> pure ()
  Engaged    -> do
    hold True
    unmask <- maskInput
    nxt    <- waitNext
    fork $ do
      recurse c nxt bs
      unmask
      hold False

-- | Await taps until the button is decided
recurse :: CanButton m
  => KeyCode                    -- ^ The keycode to which this button is mapped
  -> m KeyEvent                 -- ^ Action that fetches the next-event
  -> [(Microseconds, Button m)] -- ^ A list of (delay, button) tuples
  -> m ()
recurse _ _   []          = pure () -- Only reachable if empty button is provided
recurse _ _   ((_, b):[]) = tap b   -- Last button: press immediately
recurse c nxt ((d, b):bs) = do      -- Recurse deeper when we encounter a press of the same code, or tap when delay runs out
  race (wait d) (waitForWith pred nxt) >>= \case
    Left  _ -> tap b
    Right _ -> recurse c nxt bs
  where
    pred e = (e^.keyCode == c) && (e^.switchState == Engaged)
