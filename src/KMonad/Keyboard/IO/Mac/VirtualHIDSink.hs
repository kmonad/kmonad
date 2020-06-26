module KMonad.Keyboard.IO.Mac.VirtualHIDSink
  ( virtualHIDSink
  )
where

import KMonad.Prelude

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.IO.Mac.Types


--------------------------------------------------------------------------------

foreign import ccall "send_key"
  send_key :: Ptr MacKeyEvent -> IO ()

-- | The VHIDSink environment
data VHIDSink = VHIDSink
  { _buffer :: Ptr MacKeyEvent -- ^ The pointer we write events to
  }
makeClassy ''VHIDSink

-- | Return a 'KeySink' using Window's @sendEvent@ functionality.
virtualHIDSink :: HasLogFunc e => RIO e (Acquire KeySink)
virtualHIDSink = mkKeySink skOpen skClose skSend

-- | Create the 'VHIDSink' environment
skOpen :: HasLogFunc e => RIO e VHIDSink
skOpen = do
  logInfo "Initializing Windows key sink"
  liftIO $ VHIDSink <$> mallocBytes (sizeOf (undefined :: MacKeyEvent))

-- | Close the 'VHIDSink' environment
skClose :: HasLogFunc e => VHIDSink -> RIO e ()
skClose sk = do
  logInfo "Closing Windows key sink"
  liftIO . free $ sk^.buffer

-- | Write an event to the pointer and prompt windows to inject it
--
-- NOTE: This can throw an error if event-conversion fails.
skSend :: HasLogFunc e => VHIDSink -> KeyEvent -> RIO e ()
skSend sk e = either throwIO go $ toMacKeyEvent e
  where go e' = liftIO $ do
          poke (sk^.buffer) e'
          send_key $ sk^.buffer
