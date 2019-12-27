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
  ( KeySink(..)
  , KeySource(..)

  -- , HasKeySink(..)
  -- , HasKeySource(..)
  )
where

import KMonad.Prelude

import KMonad.Keyboard
import KMonad.Util



--------------------------------------------------------------------------------
-- $types

-- | An 'Emitter' is a function that emits 'KeyEvent's to the OS
newtype KeySink = KeySink { emitKeyWith :: KeyAction -> IO () }

-- | A 'Receiver' is an action that awaits 'KeyEvent's from the OS
newtype KeySource = KeySource { awaitKeyWith :: IO (Timed KeyAction)}


--------------------------------------------------------------------------------

-- -- | Wait for the next 'KeyEvent' to occur using the 'KeySource'
-- awaitKey :: HasKeySource e => RIO e KeyEvent
-- awaitKey = liftIO =<< awaitKeyWith <$> view keySource

-- -- | Send a key action to the OS using the 'KeySink'
-- emitKey :: HasKeySink e => KeyAction -> RIO e ()
-- emitKey k = emitKeyWith <$> view keySink >>= \emit -> liftIO (emit k)






-- --------------------------------------------------------------------------------
-- -- $err

-- type SinkId = String

-- -- | The type of things that can go wrong with KeyIO
-- --
-- -- TODO: rewrap basic IO errors in a KeyIO error (stuff like "file not found")
-- data KeyIOError
--   = IOCtlGrabError    FilePath
--   | IOCtlReleaseError FilePath
--   | EventParseError   FilePath String
--   | SinkCreationError SinkId
--   | SinkDeletionError SinkId
--   | SinkWriteError    SinkId   KeyEvent
--   deriving Exception

-- instance Show KeyIOError where
--   show (IOCtlGrabError pth)
--     = "Could not perform IOCTL grab on: " <> pth
--   show (IOCtlReleaseError pth)
--     = "Could not perform IOCTL release on: " <> pth
--   show (EventParseError pth snk)
--     = concat [ "Error parsing event when reading from "
--              , pth
--              , ": "
--              , snk ]
--   show (SinkCreationError snk)
--     = "Error instantiating KeySink of type: " <> snk
--   show (SinkDeletionError snk)
--     = "Error deleting KeySink of type: " <> snk
--   show (SinkWriteError snk e)
--     = concat [ "Error writing event '"
--              , unpack $ pretty e
--              , "'to KeySink: "
--              , snk ]

-- -- | Classy Prisms to create and throw 'KeyIOError's
-- makeClassyPrisms ''KeyIOError

-- instance AsKeyIOError IOException where
--   _KeyIOError = prism' throw (const Nothing)

-- -- | They type of things that can perform KeyIO
-- type CanKeyIO e m = (AsKeyIOError e, MonadError e m, MonadIO m)
