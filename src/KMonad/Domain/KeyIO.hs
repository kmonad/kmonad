{-|
Module      : KMonad.Domain.KeyIO
Description : The logic behind sending and receiving key events to the OS
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Domain.KeyIO
  ( KeySink(..)
  , KeySource(..)

  , HasKeySink(..)
  , HasKeySource(..)

  , awaitKey
  , emitKey
  )
where

import KMonad.Prelude

import KMonad.Core




--------------------------------------------------------------------------------
-- $types

-- | An 'Emitter' is a function that emits 'KeyEvent's to the OS
newtype KeySink = KeySink { emitKeyWith :: KeyAction -> IO () }

class HasKeySink a where keySink :: Lens' a KeySink

-- | A 'Receiver' is an action that awaits 'KeyEvent's from the OS
newtype KeySource = KeySource { awaitKeyWith :: IO KeyEvent }

class HasKeySource a where keySource :: Lens' a KeySource

--------------------------------------------------------------------------------

-- | Wait for the next 'KeyEvent' to occur using the 'KeySource'
awaitKey :: HasKeySource e => RIO e KeyEvent
awaitKey = liftIO =<< awaitKeyWith <$> view keySource

-- | Send a key action to the OS using the 'KeySink'
emitKey :: HasKeySink e => KeyAction -> RIO e ()
emitKey k = emitKeyWith <$> view keySink >>= \emit -> liftIO (emit k)





--------------------------------------------------------------------------------
-- $esrc
--
-- We cannot work with KeySource objects directly for awaiting 'Event's, as
-- 'srcRead' is a blocking call. Instead we wrap a 'KeySource' in an
-- 'EventSource', which manages input asynchronously, and allows waiting for
-- events from the keyboard while providing a mechanism to inject 'Event's into
-- KMonad's event-loop.

-- | An object that manages a KeySource and allows async injecting of other events
-- data EventSource = EventSource
--   { _waitKey :: Receiver
--   , _injectV :: MVar Event
--   }
-- -- | A classy lens to having an 'EventSource'
-- makeClassy ''EventSource

-- -- | Create a new EventSource by wrapping a receiver
-- mkEventSource :: MonadIO m => Receiver -> m EventSource
-- mkEventSource nextKey = EventSource nextKey <$> newEmptyMVar

-- -- | Await the next event from an EventSource, this blocks until an event occurs
-- readEvent :: MonadIO m => EventSource -> m Event
-- readEvent es =
--   liftIO $ U.race (KIOEvent <$> es^.waitKey) (takeMVar $ es^.injectV) >>= \case
--     Left e  -> return e
--     Right e -> return e

-- -- | Inject a new event into the EventSource, can block if the events aren't
-- -- being handled fast enough.
-- writeEvent :: MonadIO m => Event -> EventSource -> m ()
-- writeEvent e es = liftIO . putMVar (es^.injectV) $ e



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
