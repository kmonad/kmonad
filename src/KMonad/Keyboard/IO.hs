{-|
Module      : KMonad.Keyboard.IO
Description : The logic behind sending and receiving key events to the OS
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Keyboard.IO
  ( -- * KeySink: send keyboard events to the OS
    -- $snk
    KeySink
  , mkKeySink
  , emitKey

    -- * KeySource: read keyboard events from the OS
  , KeySource
  , mkKeySource
  , awaitKey
  )
where

import KMonad.Prelude

import KMonad.Keyboard.Types
import KMonad.Util

import qualified RIO.Text as T

--------------------------------------------------------------------------------
-- $snk

-- | A 'KeySink' sends key actions to the OS
newtype KeySink = KeySink { emitKeyWith :: KeyEvent -> IO () }

-- | Create a new 'KeySink'
mkKeySink :: HasLogFunc e
  => RIO e snk                      -- ^ Action to acquire the keysink
  -> (snk -> RIO e ())              -- ^ Action to close the keysink
  -> (snk -> KeyEvent -> RIO e ())  -- ^ Action to write to the keysink
  -> RIO e (Acquire KeySink)
mkKeySink o c w = do
  u     <- askUnliftIO
  let open        = unliftIO u $ logInfo "Opening KeySink" >> o
  let close snk   = unliftIO u $ logInfo "Closing KeySink" >> c snk
  let write snk a = unliftIO u $ w snk a
        `catch` logRethrow "Encountered error in KeySink"
  pure $ KeySink . write <$> mkAcquire open close

-- | Emit a key to the OS
emitKey :: (HasLogFunc e) => KeySink -> KeyEvent -> RIO e ()
emitKey snk e = do
  logDebug $ "Emitting: " <> display e
  liftIO $ emitKeyWith snk e


--------------------------------------------------------------------------------
-- $src

-- | A 'KeySource' is an action that awaits 'KeyEvent's from the OS
newtype KeySource = KeySource { awaitKeyWith :: IO KeyEvent}

-- | Create a new KeySource
mkKeySource :: HasLogFunc e
  => RIO e src               -- ^ Action to acquire the keysource
  -> (src -> RIO e ())       -- ^ Action to close the keysource
  -> (src -> RIO e KeyEvent) -- ^ Action to read from the keysource
  -> RIO e (Acquire KeySource)
mkKeySource o c r = do
  u <- askUnliftIO
  let open      = unliftIO u $ logInfo "Opening KeySource" >> o
  let close src = unliftIO u $ logInfo "Closing KeySource" >> c src
  let read src  = unliftIO u $ r src
        `catch` logRethrow "Encountered error in KeySource"
  pure $ KeySource . read <$> mkAcquire open close

-- | Wait for the next key from the OS
awaitKey :: (HasLogFunc e) => KeySource -> RIO e KeyEvent
awaitKey src = do
  e <- liftIO . awaitKeyWith $ src
  logDebug $ "\n" <> display (T.replicate 80 "-")
          <> "\nReceived event: " <> display e
  pure e
