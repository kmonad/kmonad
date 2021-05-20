module KMonad.App.Main.Subtasks

where

{- NOTE:

The normal 'Types', 'IO', 'Operations', subdivision does not make a lot of sense
for 'Main', since *everything* is IO. However, since the module was getting
rather big, here are subroutines for subtasks.

-}


import KMonad.Prelude
import KMonad.Util.Keyboard
import KMonad.Util.Logging
import KMonad.App.Types

-- | Get the next 'KeyEvent' from the OS
--
-- This labels the received 'KeySwitch' value with the time this action
-- completed, and provides logging at the 'LevelDebug' level, for more
-- introspection.
getKey :: (EnvIO m e, CanK e) => m KeyEvent
getKey = do
  nxt <- join $ fmap liftIO $ view keySource
  sepDebug >> logDebug ("Received KeyEvent from OS: " <> dsp nxt)
  keyEventNow nxt

-- | Send a 'KeySwitch' to the OS
--
-- This takes anything that can be viewed as a 'KeySwitch' and extracts said
-- 'KeySwitch' and sends it to the OS using the 'keySink'. It provides logging
-- at 'LevelDebug' levels, for more introspection.
putKey :: (EnvIO m e, CanK e, HasKeySwitch a) => a -> m ()
putKey a = do
  logDebug $ "Sending KeySwitch to the OS: " <> dsp (a^.keySwitch)
  snk <- view keySink
  liftIO $ snk $ a^.keySwitch
