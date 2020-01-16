{-# LANGUAGE DeriveAnyClass #-}
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
  , HasKeySink
  , keySink
  , mkKeySink
  , emitKey

    -- * KeySource: read keyboard events from the OS
  , KeySource
  , HasKeySource
  , keySource
  , mkKeySource
  , awaitKey
  )
where

import KPrelude

import KMonad.Keyboard
import KMonad.Util

import qualified RIO.Text as T

--------------------------------------------------------------------------------
-- $snk

-- | A 'KeySink' sends key actions to the OS
newtype KeySink = KeySink { emitKeyWith :: KeyAction -> IO () }
makeClassy ''KeySink

-- | Create a new 'KeySink'
mkKeySink :: HasLogFunc e
  => RIO e snk                      -- ^ Action to acquire the keysink
  -> (snk -> RIO e ())              -- ^ Action to close the keysink
  -> (snk -> KeyAction -> RIO e ()) -- ^ Action to write with the keysink
  -> RIO e (Acquire KeySink)
mkKeySink o c w = do
  u     <- askUnliftIO
  let open        = unliftIO u $ logInfo "Opening KeySink" >> o
  let close snk   = unliftIO u $ logInfo "Closing KeySink" >> c snk
  let write snk a = unliftIO u $ w snk a
        `catch` logRethrow "Encountered error in KeySink"
  pure $ KeySink . write <$> mkAcquire open close

-- | Emit a key to the OS
emitKey :: (HasLogFunc e, HasKeySink e) => KeyAction -> RIO e ()
emitKey a = do
  logDebug $ "Emitting: " <> display a
  view keySink >>= liftIO . flip emitKeyWith a



--------------------------------------------------------------------------------
-- $src

-- | A 'KeySource' is an action that awaits 'KeyEvent's from the OS
newtype KeySource = KeySource { awaitKeyWith :: IO KeyEvent }
makeClassy ''KeySource

-- | Create a new KeySource
mkKeySource :: HasLogFunc e
  => RIO e src               -- ^ Action to acquire the keysink
  -> (src -> RIO e ())       -- ^ Action to close the keysink
  -> (src -> RIO e KeyEvent) -- ^ Action to write with the keysink
  -> RIO e (Acquire KeySource)
mkKeySource o c r = do
  u <- askUnliftIO
  let open      = unliftIO u $ logInfo "Opening KeySource" >> o
  let close src = unliftIO u $ logInfo "Closing KeySource" >> c src
  let read src  = unliftIO u $ r src
        `catch` logRethrow "Encountered error in KeySource"
  pure $ KeySource . read <$> mkAcquire open close

-- | Wait for the next key from the OS
awaitKey :: (HasLogFunc e, HasKeySource e) => RIO e KeyEvent
awaitKey = do
  e <- liftIO . awaitKeyWith =<< view keySource
  logDebug $ "\n" <> display (T.replicate 80 "-")
          <> "\nReceived event: " <> display e
  pure e
