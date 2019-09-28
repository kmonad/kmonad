{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Api.KeyIO.Types
Description : The interface between concrete IO and App data
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This module defines an interface used to plug KeyIO functionality into App, and
KMonad. This means that if you want to extend KMonad by adding other key sources
or sinks, you must simple make sure your module provides a KeySource or KeySink
object that matches the types defined here, then KMonad will use this object to
interface with the OS.

-}
module KMonad.Api.KeyIO.Types
  ( -- * Common API to any type of OS keyboard interface
    -- $types
    KeySink(..)
  , KeySource(..)
  , withKeySource
  , withKeySink
  , Emitter
  , Receiver
  , HasEmitter(..)
  , HasEventSource(..)
  , CanKeyIO

    -- * Wrapped KeySource that allows injecting events
    -- $esrc
  , EventSource
  , mkEventSource
  , readEvent
  , writeEvent

    -- * Collection of things that can go wrong with KeyIO
    -- $err
  , KeyIOError
  , AsKeyIOError(..)
  )
where

import Prelude hiding (read)

import Control.Exception (Exception, throw)
import Control.Lens
import Control.Monad.Except
import Data.Text (unpack)

import UnliftIO as U


import KMonad.Core


--------------------------------------------------------------------------------
-- $types
--
--

-- | The type of functions that emit key events to the OS
type Emitter = KeyEvent -> IO ()

-- | The type of actions that receive new key events
type Receiver = IO KeyEvent

-- | A record describing how to plug in Key-writing capabilities
data KeySink = forall a. KeySink
  { snkOpen  :: IO a         -- ^ How to open and capture the output
  , snkWrite :: a -> Emitter -- ^ How to write events to the output
  , snkClose :: a -> IO ()   -- ^ How to release and close the output
  }

-- | A record describing how to plug in Key-reading capabilities
data KeySource = forall a. KeySource
  { srcOpen  :: IO a          -- ^ How to open and capture the input
  , srcRead  :: a -> Receiver -- ^ How to read events from the input
  , srcClose :: a -> IO ()    -- ^ How to close and release the input
  }

-- | Run a function that writes key events in the context of an acquired
-- KeySink. This uses bracket functionality to make sure the cleanup is always
-- performed, even on error.
withKeySink :: KeySink -> (Emitter -> IO a) ->  IO a
withKeySink KeySink{snkOpen=open, snkClose=close, snkWrite=write} go
  = bracket open close (\a -> go $ write a)

-- | Run a function that reads key events in the context of an acquired
-- KeySource. This uses bracket functionality to make sure the cleanup is always
-- performed.
withKeySource :: KeySource -> (Receiver -> IO a) -> IO a
withKeySource KeySource{srcOpen=open, srcClose=close, srcRead=read} go
  = bracket open close (\a -> go $ read a)

-- | The property of having access to an Emitter function
class HasEmitter r where
  emitter :: Lens' r Emitter


--------------------------------------------------------------------------------
-- $esrc
--
-- We cannot work with KeySource objects directly for awaiting 'Event's, as
-- 'srcRead' is a blocking call. Instead we wrap a 'KeySource' in an
-- 'EventSource', which manages input asynchronously, and allows waiting for
-- events from the keyboard while providing a mechanism to inject 'Event's into
-- KMonad's event-loop.

-- | An object that manages a KeySource and allows async injecting of other events
data EventSource = EventSource
  { _waitKey :: Receiver
  , _injectV :: MVar Event
  }
-- | A classy lens to having an 'EventSource'
makeClassy ''EventSource

-- | Create a new EventSource by wrapping a receiver
mkEventSource :: MonadIO m => Receiver -> m EventSource
mkEventSource nextKey = EventSource nextKey <$> newEmptyMVar

-- | Await the next event from an EventSource, this blocks until an event occurs
readEvent :: MonadIO m => EventSource -> m Event
readEvent es =
  liftIO $ U.race (InputEvent <$> es^.waitKey) (takeMVar $ es^.injectV) >>= \case
    Left e  -> return e
    Right e -> return e

-- | Inject a new event into the EventSource, can block if the events aren't
-- being handled fast enough.
writeEvent :: MonadIO m => Event -> EventSource -> m ()
writeEvent e es = liftIO . putMVar (es^.injectV) $ e


--------------------------------------------------------------------------------
-- $err

type SinkId = String

-- | The type of things that can go wrong with KeyIO
--
-- TODO: rewrap basic IO errors in a KeyIO error (stuff like "file not found")
data KeyIOError
  = IOCtlGrabError FilePath
  | IOCtlReleaseError FilePath
  | EventParseError FilePath String
  | SinkCreationError SinkId
  | SinkDeletionError SinkId
  | SinkWriteError SinkId KeyEvent
  deriving Exception

instance Show KeyIOError where
  show (IOCtlGrabError pth)
    = "Could not perform IOCTL grab on: " <> pth
  show (IOCtlReleaseError pth)
    = "Could not perform IOCTL release on: " <> pth
  show (EventParseError pth s)
    = concat [ "Error parsing event when reading from "
             , pth
             , ": "
             , s ]
  show (SinkCreationError s)
    = "Error instantiating KeySink of type: " <> s
  show (SinkDeletionError s)
    = "Error deleting KeySink of type: " <> s
  show (SinkWriteError s e)
    = concat [ "Error writing event '"
             , unpack $ pretty e
             , "'to KeySink: "
             , s ]

-- | Classy Prisms to create and throw 'KeyIOError's
makeClassyPrisms ''KeyIOError

instance AsKeyIOError IOException where
  _KeyIOError = prism' throw (const Nothing)

-- | They type of things that can perform KeyIO
type CanKeyIO e m = (AsKeyIOError e, MonadError e m, MonadIO m)
