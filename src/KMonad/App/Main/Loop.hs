module KMonad.App.Main.Loop

where

import KMonad.Prelude

import KMonad.Model
import KMonad.Util
import KMonad.App.Types

-- | Run KMonad's app loop
loop :: App ()
loop = runCtx_ writer reader where
  reader = forever $ getKey >>= sendToModel
  writer = do
    around (logInfo "Launching emitter-process thread")
           (logInfo "Closing emitter-process thread")
    launch_ $ recvFromModel >>= putKey

-- | Get the next 'KeyEvent' from the OS
getKey :: App KeyEvent
getKey = do
  nxt <- join $ fmap liftIO $ view keySource
  sepDebug >> logDebug ("Got: " <> dsp nxt)
  keyEventNow nxt

-- | Send a 'KeySwitch' to the OS
putKey :: HasKeySwitch a => a -> App ()
putKey a = do
  logDebug $ "Put: " <> dsp (a^.keySwitch)
  snk <- view keySink
  liftIO $ snk $ a^.keySwitch
