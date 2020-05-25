{-|
Module      : KMonad.Keyboard.IO.Windows.SendEventSink
Description : Using Windows' send_event functionality to inject KeyEvent's
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This uses @sendKey@ from the @keyio_win.c@ to send keys to Windows. This itself
then uses the Windows 'SendInput' system call.

-}
module KMonad.Keyboard.IO.Windows.SendEventSink
  ( sendEventKeySink
  )
where

import KMonad.Prelude

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.IO.Windows.Types


--------------------------------------------------------------------------------

foreign import ccall "sendKey"
  sendKey :: Ptr WinKeyEvent -> IO ()

-- | The SKSink environment
data SKSink = SKSink
  { _buffer :: Ptr WinKeyEvent -- ^ The pointer we write events to
  }
makeClassy ''SKSink

-- | Return a 'KeySink' using Window's @sendEvent@ functionality.
sendEventKeySink :: HasLogFunc e => RIO e (Acquire KeySink)
sendEventKeySink = mkKeySink skOpen skClose skSend

-- | Create the 'SKSink' environment
skOpen :: HasLogFunc e => RIO e SKSink
skOpen = do
  logInfo "Initializing Windows key sink"
  liftIO $ SKSink <$> mallocBytes (sizeOf (undefined :: WinKeyEvent))

-- | Close the 'SKSink' environment
skClose :: HasLogFunc e => SKSink -> RIO e ()
skClose sk = do
  logInfo "Closing Windows key sink"
  liftIO . free $ sk^.buffer

-- | Write an event to the pointer and prompt windows to inject it
--
-- NOTE: This can throw an error if event-conversion fails.
skSend :: HasLogFunc e => SKSink -> KeyEvent -> RIO e ()
skSend sk e = either throwIO go $ toWinKeyEvent e
  where go e' = liftIO $ do
          poke (sk^.buffer) e'
          sendKey $ sk^.buffer
