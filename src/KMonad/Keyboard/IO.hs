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
  , KeySource(..)

  , HasKeySink(..)
  , emitKey

    -- * A collection of things that can go wrong with Key IO
    -- $err
  , KeyIOError(..)
  )
where

import KMonad.Prelude

import KMonad.Keyboard
import KMonad.Util



--------------------------------------------------------------------------------
-- $types

-- | An 'Emitter' is a function that emits 'KeyEvent's to the OS
newtype KeySink = KeySink { emitKeyWith :: KeyAction -> IO () }
makeClassy ''KeySink

-- | A 'Receiver' is an action that awaits 'KeyEvent's from the OS
newtype KeySource = KeySource { awaitKeyWith :: IO (Timed KeyAction)}


--------------------------------------------------------------------------------

-- -- | Wait for the next 'KeyEvent' to occur using the 'KeySource'
-- awaitKey :: HasKeySource e => RIO e KeyEvent
-- awaitKey = liftIO =<< awaitKeyWith <$> view keySource

-- | Send a key action to the OS using the 'KeySink'
emitKey :: HasKeySink e => KeyAction -> RIO e ()
emitKey k = emitKeyWith <$> view keySink >>= \emit -> liftIO (emit k)


--------------------------------------------------------------------------------
-- $err

type SinkId = String

-- | The type of things that can go wrong with KeyIO
data KeyIOError
  = IOCtlGrabError       FilePath
  | IOCtlReleaseError    FilePath
  | EventParseError      FilePath String
  | SinkCreationError    SinkId
  | SinkDeletionError    SinkId
  | SinkTranslationError KeyEvent
  | SinkWriteError       SinkId


instance Exception KeyIOError

instance Show KeyIOError where
  show (IOCtlGrabError pth)
    = "Could not perform IOCTL grab on: " <> pth
  show (IOCtlReleaseError pth)
    = "Could not perform IOCTL release on: " <> pth
  show (EventParseError pth snk)
    = concat [ "Error parsing event when reading from "
             , pth
             , ": "
             , snk ]
  show (SinkCreationError snk)
    = "Error instantiating KeySink of type: " <> snk
  show (SinkDeletionError snk)
    = "Error deleting KeySink of type: " <> snk
  show (SinkTranslationError e)
    = "Could not translate event to OS-style: " <> show e
  show (SinkWriteError snk)
    = "Could not write to sink: " <> snk
