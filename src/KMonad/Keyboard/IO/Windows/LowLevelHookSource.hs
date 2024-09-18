{-|
Module      : KMonad.Keyboard.IO.Windows.LowLevelHookSource
Description : Load and acquire a windows low-level keyboard hook.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Keyboard.IO.Windows.LowLevelHookSource
  ( llHook
  )
where

import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable

import KMonad.Keyboard
import KMonad.Keyboard.IO.Windows.Types

--------------------------------------------------------------------------------

-- | Use the windows c-api to `grab` a keyboard
foreign import ccall "grab_kb"
  grab_kb :: IO ()

-- | Release the keyboard hook
foreign import ccall "release_kb"
  release_kb :: IO ()

-- | Pass a pointer to a buffer to wait_key, when it returns the buffer can be
-- read for the next key event.
foreign import ccall "wait_key"
  wait_key :: Ptr WinKeyEvent -> IO ()


--------------------------------------------------------------------------------

-- | Data used to track `connection` to windows process
data LLHook = LLHook
  { _thread :: !(Async ())        -- ^ The thread-id of the listen-process
  , _buffer :: !(Ptr WinKeyEvent) -- ^ Buffer used to communicate with process
  }

-- | Return a KeySource using the Windows low-level hook approach.
llHook :: HasLogFunc e => RIO e (Acquire KeySource)
llHook = mkKeySource llOpen llClose llRead


--------------------------------------------------------------------------------

-- | Ask windows to install a hook and allocate the reading-buffer
llOpen :: HasLogFunc e => RIO e LLHook
llOpen = do
  logInfo "Registering low-level Windows keyboard hook"
  liftIO $ LLHook <$> async grab_kb <*> malloc

-- | Ask windows to unregister the hook and free the data-buffer
llClose :: HasLogFunc e => LLHook -> RIO e ()
llClose (LLHook tid buf) = do
  logInfo "Unregistering low-level Windows keyboard hook"
  -- Cancelling the thread might not be necessary, but it is safer
  liftIO $ release_kb *> cancel tid *> free buf

-- | Get a new 'KeyEvent' from Windows
--
-- NOTE: This can throw an error if the event fails to convert.
llRead :: LLHook -> RIO e KeyEvent
llRead (LLHook{_buffer = buf}) = do
  we <- liftIO $ wait_key buf *> peek buf
  either throwIO pure $ fromWinKeyEvent we
