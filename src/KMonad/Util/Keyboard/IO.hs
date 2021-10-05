module KMonad.Util.Keyboard.IO

where

import KMonad.Prelude
import KMonad.Util.Time
import KMonad.Util.Keyboard.Types

-- | Create a 'KeyEvent' for the current time.
--
-- This can be used to turn a 'KeySwitch' into a 'KeyEvent', or to update a
-- 'KeyEvent' to the current time.
keyEventNow :: (HasCode s, HasSwitch s, IO m) => s -> m KeyEvent
keyEventNow s = now $ mkKeyEvent (s^.switch) (s^.code)
