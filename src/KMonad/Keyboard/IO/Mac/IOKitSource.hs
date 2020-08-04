module KMonad.Keyboard.IO.Mac.IOKitSource
  ( iokitSource
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
iokitSource :: HasLogFunc e
  => (Maybe String)
  -> RIO e (Acquire KeySource)
iokitSource name = mkKeySource (iokitOpen name) iokitClose iokitRead


--------------------------------------------------------------------------------

-- | Ask IOKit to open keyboards matching the specified name
iokitOpen :: HasLogFunc e
  => (Maybe String)
  -> RIO e EvBuf
iokitOpen m = do
  logInfo "Opening IOKit devices"
  liftIO $ do

    case m of
      Nothing -> void $ grab_kb nullPtr
      Just s  -> do
        str <- newCString s
        void $ grab_kb str
        free str

    buf <- mallocBytes $ sizeOf (undefined :: MacKeyEvent)
    pure $ EvBuf buf

-- | Ask Mac to close the queue
iokitClose :: HasLogFunc e => EvBuf -> RIO e ()
iokitClose b = do
  logInfo "Closing IOKit devices"
  liftIO $ do
    _ <- release_kb
    free $ b^.buffer

-- | Get a new 'KeyEvent' from Mac
--
-- NOTE: This can throw an error if the event fails to convert.
iokitRead :: HasLogFunc e => EvBuf -> RIO e KeyEvent
iokitRead b = do
  we <- liftIO $ do
    _ <- wait_key $ b^.buffer
    peek $ b^.buffer
  either throwIO pure $ fromMacKeyEvent we
