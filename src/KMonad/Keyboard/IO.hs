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
  ( -- * The basic types of Key IO
    -- $types
    KeySink(..)
  , SinkId
  , mkKeySink
  , emitKeyWith

  , KeySource
  , mkKeySource
  , awaitKeyWith

  , HasKeySink(..)

    -- * A collection of things that can go wrong with Key IO
    -- $err
    
  )
where

import Prelude

import KMonad.Keyboard
import KMonad.Util



--------------------------------------------------------------------------------
-- $types

-- | An 'Emitter' is a function that emits 'KeyEvent's to the OS
newtype KeySink = KeySink { _emitKeyWith :: KeyAction -> RIO RunEnv () }

-- | Create a new 'KeySink'
mkKeySink :: HasRunEnv e
  => RIO RunEnv a                      -- ^ Action to open the keysink
  -> (a -> RIO RunEnv ())              -- ^ Action to close the keysink
  -> (a -> KeyAction -> RIO RunEnv ()) -- ^ Action to read from the keysink
  -> RIO e (Acquire KeySink)
mkKeySink open close write = do
  renv <- view runEnv
  pure $ KeySink . write <$> mkAcquire
    (runRIO renv $ logInfo "Opening KeySink" >> open)
    (runRIO renv . (\d -> logInfo "Closing KeySink" >> close d))

-- | Send a key action to the OS using the 'KeySink'
emitKeyWith :: HasRunEnv e => KeySink -> KeyAction -> RIO e ()
emitKeyWith snk k = view runEnv >>= flip runRIO (_emitKeyWith snk k)

-- | A 'KeySource' is an action that awaits 'KeyEvent's from the OS
newtype KeySource = KeySource { _awaitKeyWith :: RIO RunEnv KeyEvent }

-- | Create a new 'KeySource'
mkKeySource :: HasRunEnv e
  => RIO RunEnv a               -- ^ Action to acquire the keysource
  -> (a -> RIO RunEnv ())       -- ^ Function to release the keysource
  -> (a -> RIO RunEnv KeyEvent) -- ^ Function to read from the keysource
  -> RIO e (Acquire KeySource)
mkKeySource open close read = do
  renv <- view runEnv
  pure $ KeySource . read <$> mkAcquire
    (runRIO renv $ logInfo "Opening KeySource" >> open)
    (runRIO renv . (\d -> logInfo "Closing KeySource" >> close d))

-- | Await a key action from the OS using the 'KeySource'
awaitKeyWith :: HasRunEnv e => KeySource -> RIO e KeyEvent
awaitKeyWith src = view runEnv >>= flip runRIO (_awaitKeyWith src)

--------------------------------------------------------------------------------

class HasRunEnv e => HasKeySink e where
  keySink :: Lens' e KeySink



--------------------------------------------------------------------------------
-- $err

type SinkId = String

