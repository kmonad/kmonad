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

foreign import ccall "sendKey" sendKey :: Ptr WinKeyEvent -> IO ()




-- | The SKSink environment
data SKSink = SKSink
  { _buffer :: MVar (Ptr WinKeyEvent) -- ^ The pointer we write events to
  , _keyrep :: MVar (Maybe (Keycode, Async ()))
  , _delay  :: Int -- ^ How long to wait before starting key repeat in ms
  , _rate   :: Int -- ^ How long to wait between key repeats in ms
  }
makeClassy ''SKSink

-- | Return a 'KeySink' using Window's @sendEvent@ functionality.
sendEventKeySink :: HasLogFunc e => Maybe (Int, Int) -> RIO e (Acquire KeySink)
sendEventKeySink di = mkKeySink (skOpen (fromMaybe (300, 100) di)) skClose skSend

-- | Create the 'SKSink' environment
skOpen :: HasLogFunc e => (Int, Int) -> RIO e SKSink
skOpen (d, i) = do
  logInfo "Initializing Windows key sink"
  bv <- liftIO $ mallocBytes (sizeOf (undefined :: WinKeyEvent))
  bm <- newMVar bv
  r <- newMVar Nothing
  pure $ SKSink bm r d i

-- | Close the 'SKSink' environment
skClose :: HasLogFunc e => SKSink -> RIO e ()
skClose s = do
  logInfo "Closing Windows key sink"
  withMVar (s^.keyrep) $ \r -> maybe (pure ()) cancel (r^?_Just._2)
  withMVar (s^.buffer) (liftIO . free)

-- | Send 1 key event to Windows
emit :: MonadUnliftIO m => SKSink -> WinKeyEvent -> m ()
emit s w = withMVar (s^.buffer) $ \b -> liftIO $ poke b w >> sendKey b

-- | Write an event to the pointer and prompt windows to inject it
--
-- NOTE: This can throw an error if event-conversion fails.
skSend :: HasLogFunc e => SKSink -> KeyEvent -> RIO e ()
skSend s e = do

  w <- either throwIO pure $ toWinKeyEvent e -- the event for windows
  r <- takeMVar $ s^.keyrep                  -- the keyrep token

   -- Whether this keycode is currently active in key-repeat
  let beingRepped = Just (e^.keycode) == (r^?_Just._1)

  -- When we're going to emit a press we are not already repeating
  let handleNewPress = do
        maybe (pure ()) cancel (r^?_Just._2)
        emit s w
        a <- async $ do
          threadDelay (1000 * s^.delay)
          forever $ emit s w >> threadDelay (1000 * s^.rate)
        pure $ Just (e^.keycode, a)

  -- When the event is a release
  let handleRelease = do
        when beingRepped $ maybe (pure ()) cancel (r^?_Just._2)
        emit s w
        pure $ if beingRepped then Nothing else r

  -- Perform the correct action and store the rep-env
  newRep <- if | isPress e && not beingRepped -> handleNewPress
               | isRelease e                  -> handleRelease
               | otherwise                    -> pure r
  putMVar (s^.keyrep) newRep
