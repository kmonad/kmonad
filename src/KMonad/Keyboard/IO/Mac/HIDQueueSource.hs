module KMonad.Keyboard.IO.Mac.HIDQueueSource
  ( hidSource
  )
where

import KMonad.Prelude

import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.IO.Mac.Types

--------------------------------------------------------------------------------

-- | Use the mac c-api to `grab` a keyboard
foreign import ccall "grab_kb"
  grab_kb :: CString -> IO Word8

-- | Release the keyboard hook
foreign import ccall "release_kb"
  release_kb :: IO Word8

-- | Pass a pointer to a buffer to wait_key, when it returns the buffer can be
-- read for the next key event.
foreign import ccall "wait_key"
  wait_key :: Ptr MacKeyEvent -> IO Word8


data EvBuf = EvBuf
  { _buffer :: !(Ptr MacKeyEvent)
  }
makeLenses ''EvBuf

-- | Return a KeySource using the Mac IOKit approach
hidSource :: HasLogFunc e
  => (Maybe String)
  -> RIO e (Acquire KeySource)
hidSource name = mkKeySource (hidOpen name) hidClose hidRead


--------------------------------------------------------------------------------

-- | Ask Mac to allocate a queue for events from keyboard HID
hidOpen :: HasLogFunc e
  => (Maybe String)
  -> RIO e EvBuf
hidOpen m = do
  logInfo "Opening HID queue"
  liftIO $ do
    str <- newCString (case m of
                       Nothing -> ""
                       Just s -> s)
    _ <- grab_kb (case m of
                    Nothing -> nullPtr
                    Just _ -> str)
    free str
    buf <- mallocBytes $ sizeOf (undefined :: MacKeyEvent)
    pure $ EvBuf buf

-- | Ask Mac to close the queue
hidClose :: HasLogFunc e => EvBuf -> RIO e ()
hidClose b = do
  logInfo "Closing HID queue"
  liftIO $ do
    _ <- release_kb
    free $ b^.buffer

-- | Get a new 'KeyEvent' from Mac
--
-- NOTE: This can throw an error if the event fails to convert.
hidRead :: HasLogFunc e => EvBuf -> RIO e KeyEvent
hidRead b = do
  we <- liftIO $ do
    _ <- wait_key $ b^.buffer
    peek $ b^.buffer
  either throwIO pure $ fromMacKeyEvent we
