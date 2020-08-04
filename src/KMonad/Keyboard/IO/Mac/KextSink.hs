module KMonad.Keyboard.IO.Mac.KextSink
  ( kextSink
  )
where

import KMonad.Prelude

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.IO.Mac.Types

foreign import ccall "send_key"
  send_key :: Ptr MacKeyEvent -> IO ()

data EvBuf = EvBuf
  { _buffer :: Ptr MacKeyEvent -- ^ The pointer we write events to
  }
makeClassy ''EvBuf

kextSink :: HasLogFunc e => RIO e (Acquire KeySink)
kextSink = mkKeySink skOpen skClose skSend

-- | Create the 'EvBuf' environment
skOpen :: HasLogFunc e => RIO e EvBuf
skOpen = do
  logInfo "Initializing Mac key sink"
  liftIO $ EvBuf <$> mallocBytes (sizeOf (undefined :: MacKeyEvent))

-- | Close the 'EvBuf' environment
skClose :: HasLogFunc e => EvBuf -> RIO e ()
skClose sk = do
  logInfo "Closing Mac key sink"
  liftIO . free $ sk^.buffer

-- | Write an event to the pointer and prompt windows to inject it
--
-- NOTE: This can throw an error if event-conversion fails.
skSend :: HasLogFunc e => EvBuf -> KeyEvent -> RIO e ()
skSend sk e = either throwIO go $ toMacKeyEvent e
  where go e' = liftIO $ do
          poke (sk^.buffer) e'
          send_key $ sk^.buffer
