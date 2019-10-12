module KMonad.Api.KeyIO.Windows.LowLevelHookSource

where

import Control.Concurrent
import Control.Lens
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import UnliftIO

import KMonad.Core
import KMonad.Api.KeyIO.Types
import KMonad.Api.KeyIO.Windows.Types


--------------------------------------------------------------------------------

-- | Use the windows c-api to `grab` a keyboard
foreign import ccall "grab_kb"
  grab_kb :: IO ()

-- | Pass a pointer to a buffer to wait_key, when it returns the buffer can be
-- read for the next key event.
foreign import ccall "wait_key"
  wait_key :: Ptr WinKeyEvent -> IO ()

-- | Return how many milliseconds ago windows was started. We use this to
-- convert windows timestamps to KMonad time stamps.
foreign import ccall "time_since_start"
  time_since_start :: IO Word32


--------------------------------------------------------------------------------

-- | Data used to track `connection` to windows process
data LLHook = LLHook
  { _thread :: !ThreadId          -- ^ The thread-id of the listen-process
  , _buffer :: !(Ptr WinKeyEvent) -- ^ Buffer used to communicate with process
  , _start  :: !Time              -- ^ Start-time of this windows session
  }
makeLenses ''LLHook

-- | Return a KeySource using the Windows low-level hook approach.
llHook :: KeySource
llHook = BracketIO
  { _open  = llOpen
  , _close = llClose
  , _use   = llRead
  }

--------------------------------------------------------------------------------

-- | Ask windows to install a hook and allocate the reading-buffer
llOpen :: CanKeyIO e m => m LLHook
llOpen = liftIO $ do
  putStrLn "starting llOpen"
  tid <- forkIO grab_kb
  buf <- mallocBytes $ sizeOf (undefined :: WinKeyEvent)
  tst <- time_since_start
  putStrLn "started llOpen"
  return $ LLHook tid buf (mkTime 0 0) -- ^ Add time tracking later

-- | Kill the thread and free the data-buffer
llClose :: CanKeyIO e m => LLHook -> m ()
llClose ll = liftIO $ do
  killThread $ ll^.thread
  free $ ll^.buffer

-- | Prompt windows to write the next event into the buffer, then read and return it.
llRead :: CanKeyIO e m => LLHook -> m KeyEvent
llRead ll = do
  liftIO . wait_key $ ll^.buffer
  we <- liftIO . peek $ ll^.buffer
  liftIO . putStrLn $ "got key event"
  liftIO . print $ we
  ee <- case we^?_KeyEvent of
          Just e  -> (liftIO $ putStrLn ("returning " <> show e)) >> return e
          Nothing -> error "Couldn't parse event" -- llRead ll
  return ee
  